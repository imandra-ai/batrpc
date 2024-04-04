open Common_
open Server_handler

type handler = Server_handler.t
type 'a with_ctx = Handler.ctx * 'a

let mk_handler_full rpc f : handler = Handler { rpc; h = Unary f }

let mk_handler rpc f : handler =
  let open Fut.Infix in
  let f_as_handler ((ctx, req) : _ Handler.with_ctx) =
    let+ res = f req in
    let new_ctx = { Handler.hmap = ctx.hmap; headers = [] } in
    new_ctx, res
  in
  mk_handler_full rpc f_as_handler

let mk_client_stream_handler rpc ~init ~on_item ~on_close () : handler =
  let h = Client_stream_handler { init; on_item; on_close } in
  Handler { rpc; h = Client_stream h }

let mk_server_stream_handler rpc f : handler =
  Handler { rpc; h = Server_stream f }

(** A type-erased stream handler, instantiated with its own state *)
type stream_handler =
  | Str_client_stream : {
      state: 'state;
      rpc: ('req, _, 'res, _) Service.Server.rpc;
      handler: ('req, 'res, 'state) client_stream_handler_with_state;
    }
      -> stream_handler

type state = {
  mutable middlewares: Middleware.t list;
  mutable services: handler Service.Server.t list;
  streams: stream_handler Int32_tbl.t;  (** Handlers for incoming streams *)
  meths: (string * handler) Str_tbl.t;
      (** Direct access to methods by their fully qualified names *)
}

type t = {
  st: state Lock.t;
  config: Framing.config;
}

let[@inline] list_services self : _ list = (Lock.get self.st).services

let show self =
  let self = Lock.get self.st in
  let show_service (s : handler Service.Server.t) = spf "%S" s.service_name in
  spf "<server [%s]>" (String.concat "," @@ List.map show_service self.services)

let pp out s = Format.pp_print_string out (show s)

let[@inline] add_middleware self m : unit =
  let@ self = Lock.with_lock self.st in
  self.middlewares <- m :: self.middlewares

let add_service_st_ (self : state) (service : handler Service.Server.t) : unit =
  let prefix =
    Namespacing.service_prefix service.package service.service_name
  in

  let add_handler (Handler { rpc; h = _ } as handler) =
    let full_name = Namespacing.assemble_meth_name ~prefix rpc.name in
    Str_tbl.replace self.meths full_name (service.service_name, handler)
  in

  self.services <- service :: self.services;
  List.iter add_handler service.handlers

let[@inline] add_service (self : t) (service : handler Service.Server.t) : unit
    =
  let@ self = Lock.with_lock self.st in
  add_service_st_ self service

let[@inline] find_meth (self : t) name : _ option =
  let@ self = Lock.with_lock self.st in
  Str_tbl.find_opt self.meths name

let create ?(middlewares = []) ~services ~framing_config:config () : t =
  let st =
    {
      middlewares;
      services = [];
      meths = Str_tbl.create 8;
      streams = Int32_tbl.create 8;
    }
  in
  List.iter (add_service_st_ st) services;
  { st = Lock.create st; config }

(** write an error response, atomically *)
let send_error (self : t) ~encoding ~(meta : Meta.meta) ~oc err : unit =
  let msg = Error.show err in
  let err = Meta.make_error ~msg () in
  let meta = Meta.make_meta ~id:meta.id ~kind:Meta.Error ~headers:[] () in

  let@ oc = Lock.with_lock oc in
  Framing.write_error ~encoding ~config:self.config oc meta err;
  oc#flush ()

let send_nothing_or_error (self : t) ~encoding ~oc ~(meta : Meta.meta)
    (res : unit Error.result) : unit =
  match res with
  | Ok () -> ()
  | Error err ->
    Log.err (fun k -> k "reply with error for id=%ld:@ %a" meta.id Error.pp err);
    send_error self ~encoding ~oc ~meta err

let send_response_or_error (self : t) ~encoding ~oc ~(meta : Meta.meta) ~rpc
    (res : _ Handler.with_ctx Error.result) : unit =
  match res with
  | Ok (ctx, res) ->
    let meta =
      Meta.make_meta ~id:meta.id ~kind:Meta.Response ~headers:ctx.headers ()
    in

    (* send response, atomically *)
    let@ oc = Lock.with_lock oc in
    Framing.write_res ~config:self.config ~encoding oc rpc meta res;
    oc#flush ()
  | Error err ->
    Log.err (fun k -> k "reply with error for id=%ld:@ %a" meta.id Error.pp err);
    send_error self ~encoding ~oc ~meta err

let send_stream_item (self : t) ~encoding ~oc ~(meta : Meta.meta) ~rpc res :
    unit =
  let meta =
    Meta.make_meta ~id:meta.id ~kind:Meta.Server_stream_item ~headers:[] ()
  in

  let@ oc = Lock.with_lock oc in
  Framing.write_res ~config:self.config ~encoding oc rpc meta res;
  oc#flush ()

let send_stream_close (self : t) ~encoding ~oc ~(meta : Meta.meta) () : unit =
  let meta =
    Meta.make_meta ~id:meta.id ~kind:Meta.Server_stream_close ~headers:[] ()
  in

  let@ oc = Lock.with_lock oc in
  Framing.write_empty ~config:self.config ~encoding oc meta ();
  oc#flush ()

let send_heartbeat (self : t) ~encoding ~oc () : unit =
  let meta = Meta.make_meta ~id:0l ~kind:Meta.Heartbeat ~headers:[] () in
  let@ oc = Lock.with_lock oc in
  Framing.write_empty ~config:self.config ~encoding oc meta ();
  oc#flush ()

let[@inline] apply_middleware ~service_name rpc (h : _ Handler.t)
    (m : Middleware.t) : _ Handler.t =
  m.handle ~service_name rpc h

let handle_request (self : t) ~encoding ~runner ~(meta : Meta.meta) ~ic ~oc () :
    unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.handle-req" in
  assert (meta.kind = Meta.Request);

  let compute_res_and_reply rpc (f : _ Handler.t) (ctx, req) : unit =
    let fut = f (ctx, req) in
    (* when [fut] is done, send result *)
    Fut.on_result fut (function
      | Ok res -> send_response_or_error self ~encoding ~oc ~meta ~rpc (Ok res)
      | Error (exn, bt) ->
        let res = Error (Error.of_exn ~kind:Errors.handler ~bt exn) in
        send_response_or_error self ~encoding ~oc ~meta ~rpc res)
  in

  let compute_stream_response f ~push_stream req : unit =
    let bt = Printexc.get_callstack 5 in

    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "rpc.server.call-server-stream-handler"
    in

    try
      f req push_stream;
      Push_stream.close push_stream
    with exn ->
      let err = Error.of_exn ~kind:Errors.handler ~bt exn in
      send_error self ~encoding ~meta ~oc err
  in

  let res : unit Error.result =
    let@ () = Error.try_catch ~kind:Errors.handler () in
    let@ () = Error.guards "Handling RPC request" in

    let meth_name =
      match meta.meth with
      | Some m -> m
      | None -> Error.fail ~kind:Errors.protocol "expected method to be present"
    in
    Log.debug (fun k -> k "(@[server.handle-req@ :method %S@])" meth_name);

    match find_meth self meth_name with
    | None ->
      Error.failf ~kind:Errors.protocol "method not found: %S." meth_name
    | Some (service_name, Handler { h = Unary initial_handler; rpc }) ->
      (* read request here, but process it in the background *)
      let req =
        Framing.read_body_req ~encoding ~config:self.config ic ~meta rpc
      in

      let handler =
        List.fold_left
          (apply_middleware ~service_name rpc)
          initial_handler (Lock.get self.st).middlewares
      in

      Runner.run_async runner (fun () ->
          let ctx = { Handler.hmap = Hmap.empty; headers = meta.headers } in
          compute_res_and_reply rpc handler (ctx, req))
    | Some
        ( _,
          Handler
            {
              rpc;
              h = Client_stream (Client_stream_handler ({ init; _ } as handler));
            } ) ->
      Framing.read_empty ~config:self.config ~encoding ic ~meta;

      (* create stream bookkeeping data *)
      let ctx = { Handler.hmap = Hmap.empty; headers = meta.headers } in
      let state = init (ctx, ()) in
      let () =
        let@ self = Lock.with_lock self.st in
        Int32_tbl.add self.streams meta.id
          (Str_client_stream { state; handler; rpc })
      in
      ()
    | Some (_, Handler { rpc; h = Server_stream f }) ->
      let req =
        Framing.read_body_req ~config:self.config ~encoding ic ~meta rpc
      in

      let closed = Atomic.make false in

      let push res : unit =
        if not (Atomic.get closed) then
          send_stream_item self ~encoding ~oc ~meta ~rpc res
      in

      let close () : unit =
        if not (Atomic.exchange closed true) then (
          Log.debug (fun k -> k "server: send stream close");
          send_stream_close self ~encoding ~oc ~meta ()
        )
      in

      let push_stream : _ Push_stream.t = { push; close } in

      Runner.run_async runner (fun () ->
          let ctx = { Handler.hmap = Hmap.empty; headers = meta.headers } in
          compute_stream_response f ~push_stream (ctx, req))
    | Some (_, Handler { rpc = _; h = Bidirectional_stream _ }) ->
      (* FIXME: handle bidirectional streams *)
      Error.failf ~kind:Errors.handler "Cannot handle method %S." meth_name
  in

  send_nothing_or_error self ~encoding ~oc ~meta res

let remove_stream_ (self : t) (id : int32) : unit =
  Log.debug (fun k -> k "(server.remove-stream@ :id %ld@]" id);
  let@ self = Lock.with_lock self.st in
  Int32_tbl.remove self.streams id

let handle_stream_close (self : t) ~encoding ~(meta : Meta.meta) ~ic ~oc () :
    unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.handle-stream-close"
  in
  assert (meta.kind = Meta.Client_stream_close);
  Framing.read_empty ~config:self.config ~encoding ic ~meta;

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

    let res : _ Handler.with_ctx Error.result =
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.call-stream.on-close"
      in
      let@ () = Error.try_catch ~kind:Errors.handler () in
      handler.on_close state
    in

    send_response_or_error self ~encoding ~oc ~meta ~rpc res

let handle_stream_item (self : t) ~encoding ~(meta : Meta.meta) ~ic ~oc () :
    unit =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.handle-stream-item"
  in
  assert (meta.kind = Meta.Client_stream_item);

  let call_handler_ state f item : unit =
    (* call handler now *)
    let res : unit Error.result =
      let@ () = Error.try_catch ~kind:Errors.handler () in
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.call-stream.on-item"
      in

      f state item
    in

    send_nothing_or_error self ~encoding ~oc ~meta res
  in

  let res : unit Error.result =
    let@ () = Error.try_catch ~kind:Errors.handler () in
    match
      let@ self = Lock.with_lock self.st in
      Int32_tbl.find_opt self.streams meta.id
    with
    | None ->
      Error.failf ~kind:Errors.protocol "No stream with id=%ld found." meta.id
    | Some (Str_client_stream { state; rpc; handler }) ->
      (* read item here, but process it in the background *)
      let item =
        Framing.read_body_req ~config:self.config ~encoding ic ~meta rpc
      in
      call_handler_ state handler.on_item item
  in

  send_nothing_or_error self ~encoding ~oc ~meta res
