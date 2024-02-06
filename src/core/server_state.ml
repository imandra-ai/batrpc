open Common_

type stream_handler =
  | Str_client_stream : {
      state: 'state;
      rpc: ('req, 'res) Service.Server.rpc;
      handler:
        ('req, 'res, 'state) Service.Server.client_stream_handler_with_state;
    }
      -> stream_handler

type state = {
  mutable middlewares: Middleware.Sync.t list;
  mutable services: Service.Server.t list;
  streams: stream_handler Int32_tbl.t;  (** Handlers for incoming streams *)
  meths: Service.Server.any_rpc Str_tbl.t;
      (** Direct access to methods by their fully qualified names *)
}

type t = { st: state Lock.t } [@@unboxed]

let[@inline] list_services self : _ list = (Lock.get self.st).services

let show self =
  let self = Lock.get self.st in
  let show_service (s : Service.Server.t) = spf "%S" s.service_name in
  spf "<server [%s]>" (String.concat "," @@ List.map show_service self.services)

let pp = Fmt.of_to_string show

let[@inline] add_middleware self m : unit =
  let@ self = Lock.with_lock self.st in
  self.middlewares <- m :: self.middlewares

let add_service_st_ (self : state) (service : Service.Server.t) : unit =
  let prefix =
    Namespacing.service_prefix service.package service.service_name
  in

  let add_handler (Service.Server.RPC rpc as any_rpc) =
    let full_name = Namespacing.assemble_meth_name ~prefix rpc.name in
    Str_tbl.replace self.meths full_name any_rpc
  in

  self.services <- service :: self.services;
  List.iter add_handler service.handlers

let[@inline] add_service (self : t) (service : Service.Server.t) : unit =
  let@ self = Lock.with_lock self.st in
  add_service_st_ self service

let[@inline] find_meth (self : t) name : _ option =
  let@ self = Lock.with_lock self.st in
  Str_tbl.find_opt self.meths name

let create ?(middlewares = []) ~services () : t =
  let st =
    {
      middlewares;
      services = [];
      meths = Str_tbl.create 8;
      streams = Int32_tbl.create 8;
    }
  in
  List.iter (add_service_st_ st) services;
  { st = Lock.create st }

(** write an error response, atomically *)
let send_error ~buf_pool ~(meta : Meta.meta) ~oc err : unit =
  let msg = Error.show err |> Ansi_clean.remove_escape_codes in
  let err = Meta.make_error ~msg () in
  let meta =
    Meta.make_meta ~id:meta.id ~kind:Meta.Error ~body_size:0l ~headers:[] ()
  in

  let@ oc = Lock.with_lock oc in
  Framing.write_error ~buf_pool oc meta err;
  oc#flush ()

let send_nothing_or_error ~buf_pool ~oc ~(meta : Meta.meta)
    (res : unit Error.result) : unit =
  match res with
  | Ok () -> ()
  | Error err -> send_error ~buf_pool ~oc ~meta err

let send_response_or_error ~buf_pool ~oc ~(meta : Meta.meta) ~rpc
    (res : _ Error.result) : unit =
  match res with
  | Ok res ->
    let meta =
      Meta.make_meta ~id:meta.id ~kind:Meta.Response ~body_size:0l ~headers:[]
        ()
    in

    (* send response, atomically *)
    let@ oc = Lock.with_lock oc in
    Framing.write_res ~buf_pool oc rpc meta res;
    oc#flush ()
  | Error err ->
    Trace.message "send error";
    send_error ~buf_pool ~oc ~meta err

let send_stream_item ~buf_pool ~oc ~(meta : Meta.meta) ~rpc res : unit =
  let meta =
    Meta.make_meta ~id:meta.id ~kind:Meta.Server_stream_item ~body_size:0l
      ~headers:[] ()
  in

  let@ oc = Lock.with_lock oc in
  Framing.write_res ~buf_pool oc rpc meta res;
  oc#flush ()

let send_stream_close ~buf_pool ~oc ~(meta : Meta.meta) () : unit =
  let meta =
    Meta.make_meta ~id:meta.id ~kind:Meta.Server_stream_close ~body_size:0l
      ~headers:[] ()
  in

  let@ oc = Lock.with_lock oc in
  Framing.write_empty ~buf_pool oc meta ();
  oc#flush ()

let handle_request (self : t) ~executor ~buf_pool ~(meta : Meta.meta) ~ic ~oc ()
    : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.server.handle-req" in
  assert (meta.kind = Meta.Request);

  let compute_res_and_reply rpc f req : unit =
    let res : _ Error.result =
      let@ () = Error.try_catch ~kind:RpcError () in
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.server.call-handler"
      in

      f req
    in

    send_response_or_error ~buf_pool ~oc ~meta ~rpc res
  in

  let compute_stream_response f ~push_stream req : unit =
    let bt = Printexc.get_callstack 5 in

    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "bin-rpc.server.call-server-stream-handler"
    in

    try
      f req push_stream;
      Service.Push_stream.close push_stream
    with exn ->
      let err = Error.of_exn_ ~kind:RpcError ~bt exn in
      send_error ~buf_pool ~meta ~oc err
  in

  let res : unit Error.result =
    let@ () = Error.guards "Handling RPC request" in
    let@ () = Error.try_catch ~kind:GenericInternalError () in

    let meth_name = meta.meth |> Error.unwrap_opt in
    Log.debug (fun k -> k "(@[server.handle-req@ :method %S@])" meth_name);

    match find_meth self meth_name with
    | None -> Error.failf ~kind:RpcError "method not found: %S." meth_name
    | Some (RPC ({ f = Service.Server.Unary f; _ } as rpc)) ->
      (* read request here, but process it in the background *)
      let req = Framing.read_body_req ~buf_pool ic ~meta rpc in
      Executor.run executor (fun () -> compute_res_and_reply rpc f req)
    | Some
        (RPC
          ({
             f =
               Service.Server.Client_stream
                 (Client_stream_handler ({ init; _ } as handler));
             _;
           } as rpc)) ->
      Framing.read_empty ~buf_pool ic ~meta;

      (* create stream bookkeeping data *)
      let state = init () in
      let () =
        let@ self = Lock.with_lock self.st in
        Int32_tbl.add self.streams meta.id
          (Str_client_stream { state; handler; rpc })
      in
      ()
    | Some (RPC ({ f = Service.Server.Server_stream f; _ } as rpc)) ->
      let req = Framing.read_body_req ~buf_pool ic ~meta rpc in

      let closed = Atomic.make false in

      let push res : unit =
        if not (Atomic.get closed) then (
          Trace.message "server: send stream item";
          send_stream_item ~buf_pool ~oc ~meta ~rpc res
        )
      in

      let close () : unit =
        if not (Atomic.exchange closed true) then (
          Trace.message "server: send stream close";
          send_stream_close ~buf_pool ~oc ~meta ()
        )
      in

      let push_stream : _ Service.Push_stream.t = { push; close } in

      Executor.run executor (fun () ->
          compute_stream_response f ~push_stream req)
    | Some (RPC { f = Service.Server.Bidirectional_stream _; _ }) ->
      Error.failf ~kind:RpcError "Cannot handle method %S." meth_name
  in

  send_nothing_or_error ~buf_pool ~oc ~meta res

let remove_stream_ (self : t) (id : int32) : unit =
  Log.debug (fun k -> k "(server.remove-stream@ :id %ld@]" id);
  let@ self = Lock.with_lock self.st in
  Int32_tbl.remove self.streams id

let handle_stream_close (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~oc () :
    unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.server.handle-stream-close"
  in
  assert (meta.kind = Meta.Client_stream_close);
  Framing.read_empty ~buf_pool ic ~meta;

  match
    let@ self = Lock.with_lock self.st in
    Int32_tbl.find_opt self.streams meta.id
  with
  | None ->
    Log.warn (fun k ->
        k "client asked to close stream %ld which is not present" meta.id)
  | Some (Str_client_stream { state; rpc; handler }) ->
    (* produce response upon closing *)
    remove_stream_ self meta.id;

    let res =
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__
          "bin-rpc.server.call-stream.on-close"
      in
      let@ () = Error.try_catch ~kind:RpcError () in
      handler.on_close state
    in

    send_response_or_error ~buf_pool ~oc ~meta ~rpc res

let handle_stream_item (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~oc () :
    unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.server.handle-stream-item"
  in
  assert (meta.kind = Meta.Client_stream_item);

  let call_handler_ state f item : unit =
    (* call handler now *)
    let res : unit Error.result =
      let@ () = Error.try_catch ~kind:RpcError () in
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.server.call-stream.on-item"
      in

      f state item
    in

    send_nothing_or_error ~oc ~buf_pool ~meta res
  in

  let res : unit Error.result =
    let@ () = Error.try_catch ~kind:RpcError () in
    match
      let@ self = Lock.with_lock self.st in
      Int32_tbl.find_opt self.streams meta.id
    with
    | None -> Error.failf ~kind:RpcError "No stream with id=%ld found." meta.id
    | Some (Str_client_stream { state; rpc; handler }) ->
      (* read item here, but process it in the background *)
      let item = Framing.read_body_req ~buf_pool ic ~meta rpc in
      call_handler_ state handler.on_item item
  in

  send_nothing_or_error ~buf_pool ~oc ~meta res
