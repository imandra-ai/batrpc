open Common_
module Push_stream = Push_stream

type 'a with_ctx = 'a Handler.with_ctx

(** A in-flight query: we're still waiting for a response. *)
type in_flight =
  | IF_unary : {
      rpc: (_, _, 'res, _) Service.Client.rpc;
      promise: 'res with_ctx Fut.promise;
      bt: Printexc.raw_backtrace;
    }
      -> in_flight
  | IF_stream : {
      rpc: (_, _, 'item, _) Service.Client.rpc;
      state: 'state;
      on_item: 'state -> 'item -> unit;
      on_close: 'state -> 'res with_ctx;
      promise: 'res with_ctx Fut.promise;
      bt: Printexc.raw_backtrace;
    }
      -> in_flight

type state = {
  mutable counter: int;  (** to allocate message numbers *)
  mutable middlewares: Middleware.t list;
  in_flight: in_flight Int32_tbl.t;
}

type t = {
  st: state Lock.t;
  encoding: Encoding.t;
  config: Framing.config;
  default_timeout_s: float;
}

let[@inline] remove_from_tbl_ (self : state) id : unit =
  Int32_tbl.remove self.in_flight id

let add_middleware self m : unit =
  let@ self = Lock.with_lock self.st in
  self.middlewares <- m :: self.middlewares

(** Handle a message of "response" type *)
let handle_response (self : t) ~(meta : Meta.meta) ~ic () : unit =
  (* Trace.message "bin-rpc.client.handle-res"; *)
  assert (meta.kind = Meta.Response);

  let config = self.config in
  let encoding = self.encoding in

  let@ self = Lock.with_lock self.st in
  match Int32_tbl.find_opt self.in_flight meta.id with
  | None ->
    Log.err (fun k ->
        k "client: received response: no request had id %ld" meta.id);
    Framing.read_and_discard ~config ~meta ~encoding ic
  | Some (IF_unary { rpc; promise; _ }) ->
    remove_from_tbl_ self meta.id;
    let res = Framing.read_body_res ~config ~encoding ic rpc ~meta in
    let ctx = { Handler.headers = meta.headers; hmap = Hmap.empty } in
    Fut.fulfill_idempotent promise (Ok (ctx, res))
  | Some (IF_stream _) ->
    remove_from_tbl_ self meta.id;
    Framing.read_and_discard ~config ~encoding ~meta ic;
    Log.err (fun k ->
        k "client: received response: expected stream item for id %ld" meta.id)

let handle_stream_item (self : t) ~(meta : Meta.meta) ~ic () : unit =
  (* Trace.message "bin-rpc.client.handle-stream-item"; *)
  assert (meta.kind = Meta.Server_stream_item);

  let config = self.config in
  let encoding = self.encoding in

  let todo =
    let@ self = Lock.with_lock self.st in
    match Int32_tbl.find_opt self.in_flight meta.id with
    | None ->
      Log.err (fun k ->
          k "client: received stream item: no request had id %ld" meta.id);
      Framing.read_and_discard ~config ~encoding ~meta ic;
      None
    | Some (IF_unary _) ->
      remove_from_tbl_ self meta.id;
      Framing.read_and_discard ~config ~encoding ~meta ic;
      Log.err (fun k ->
          k "client: received stream item: expected response for id %ld" meta.id);
      None
    | Some (IF_stream { rpc; state; on_item; _ }) ->
      let res = Framing.read_body_res ~config ~encoding ic rpc ~meta in
      Some (fun () -> on_item state res)
  in

  match todo with
  | None -> ()
  | Some f -> f ()

let handle_stream_close (self : t) ~(meta : Meta.meta) ~ic () : unit =
  (* Trace.message "bin-rpc.client.handle-stream-close"; *)
  assert (meta.kind = Meta.Server_stream_close);

  let config = self.config in
  let encoding = self.encoding in

  let todo =
    let@ self = Lock.with_lock self.st in
    match Int32_tbl.find_opt self.in_flight meta.id with
    | None ->
      Framing.read_and_discard ~config ~encoding ~meta ic;
      Log.err (fun k ->
          k "client: received stream-close: no request had id %ld" meta.id);
      None
    | Some (IF_unary _) ->
      remove_from_tbl_ self meta.id;
      Framing.read_and_discard ~config ~encoding ~meta ic;
      Log.err (fun k ->
          k "client: received stream item: expected response for id %ld" meta.id);
      None
    | Some (IF_stream { on_close; state; promise; _ }) ->
      remove_from_tbl_ self meta.id;
      Framing.read_empty ~config ~encoding ~meta ic;
      Some
        (fun () ->
          let res = on_close state in
          Fut.fulfill_idempotent promise (Ok res))
  in

  match todo with
  | None -> ()
  | Some f -> f ()

(** Handle a message of "error" type *)
let handle_error (self : t) ~(meta : Meta.meta) ~ic () : unit =
  let config = self.config in
  let encoding = self.encoding in

  let@ self = Lock.with_lock self.st in
  match Int32_tbl.find_opt self.in_flight meta.id with
  | None ->
    Framing.read_and_discard ~config ~encoding ~meta ic;
    Log.err (fun k -> k "client: received error: no request had id %ld" meta.id)
  | Some (IF_unary { promise; bt; _ }) ->
    remove_from_tbl_ self meta.id;
    let err = Framing.read_error ~config ~encoding ic ~meta in
    let err = Error.mk_error ~kind:Errors.remote err.msg in
    Log.err (fun k ->
        k "client: received error for id %ld:@ %a" meta.id Error.pp err);
    Fut.fulfill_idempotent promise (Error (Error.E err, bt))
  | Some (IF_stream { bt; promise; _ }) ->
    remove_from_tbl_ self meta.id;
    let err = Framing.read_error ~config ~encoding ic ~meta in
    let err = Error.mk_error ~kind:Errors.remote err.msg in
    Log.err (fun k ->
        k "client: received error for id %ld:@ %a" meta.id Error.pp err);
    Fut.fulfill_idempotent promise (Error (Error.E err, bt))

let handle_timeout (self : t) id : unit =
  let@ self = Lock.with_lock self.st in
  let entry = Int32_tbl.find_opt self.in_flight id in
  Option.iter
    (function
      | IF_unary { promise; bt; _ } ->
        remove_from_tbl_ self id;
        let err = Error.mk_error ~kind:Error_kind.timeout "Timeout" in
        Log.err (fun k -> k "client: timeout for id %ld:" id);
        Fut.fulfill_idempotent promise (Error (Error.E err, bt))
      | IF_stream { promise; bt; _ } ->
        remove_from_tbl_ self id;
        let err = Error.mk_error ~kind:Error_kind.timeout "Timeout" in
        Log.err (fun k -> k "client: timeout for id %ld" id);
        Fut.fulfill_idempotent promise (Error (Error.E err, bt)))
    entry

let[@inline] apply_middleware rpc (h : _ Handler.t) (m : Middleware.t) :
    _ Handler.t =
  m.handle rpc h

let create ?(middlewares = []) ~default_timeout_s ~framing_config:config
    ~encoding () : t =
  {
    config;
    default_timeout_s;
    encoding;
    st =
      Lock.create
        {
          (* leave id=0 for hearbeats and the likes *)
          counter = 1;
          middlewares;
          in_flight = Int32_tbl.create 8;
        };
  }

(** Call [f] with a protobuf encoder *)
let[@inline] with_pbrt_enc_ (self : t) f =
  Buf_pool.with_enc self.config.buf_pool f

let check_timeout_ timeout_s =
  if timeout_s < 0.001 then invalid_arg "BatRPC.Client: timeout must be >= 1ms."

let prepare_query_ (self : state) ~(rpc : _ Service.Client.rpc) ~headers
    ~in_flight () : int32 * Meta.meta =
  (* prepare query *)
  let id = Int32.of_int self.counter in
  self.counter <- 1 + self.counter;

  Int32_tbl.add self.in_flight id in_flight;

  let meth : string =
    let service_prefix =
      Namespacing.service_prefix rpc.package rpc.service_name
    in
    Namespacing.assemble_meth_name ~prefix:service_prefix rpc.rpc_name
  in

  let meta = Meta.make_meta ~id ~meth:(Some meth) ~kind:Request ~headers () in
  id, meta

(** Write request to [oc] in as small a critical section as possible *)
let send_request_ (self : t) ~oc ~meta ~rpc req : unit =
  let@ enc = with_pbrt_enc_ self in
  Pbrt.Encoder.clear enc;

  let@ oc = Lock.with_lock oc in
  Framing.write_req ~enc ~config:self.config ~encoding:self.encoding oc rpc meta
    req;
  oc#flush ()

let mk_unary_handler (self : t) ~timer ~oc ~timeout_s rpc : _ Handler.t =
 fun (ctx, req) : _ Fut.t ->
  (* TODO: can we just avoid that? *)
  let bt = Printexc.get_callstack 5 in

  check_timeout_ timeout_s;
  let fut, promise = Fut.make () in

  let in_flight = IF_unary { rpc; promise; bt } in

  let id, meta =
    (* critical section to update internal state *)
    let@ self = Lock.with_lock self.st in
    prepare_query_ self ~rpc ~headers:ctx.headers ~in_flight ()
  in

  (* setup timeout *)
  Timer.run_after_s timer timeout_s (fun () -> handle_timeout self id);

  send_request_ self ~rpc ~oc ~meta req;
  fut

let send_heartbeat (self : t) ~oc () : unit =
  let meta = Meta.make_meta ~id:0l ~kind:Meta.Heartbeat ~headers:[] () in
  let@ oc = Lock.with_lock oc in
  Framing.write_empty ~config:self.config ~encoding:self.encoding oc meta ();
  oc#flush ()

let get_timeout_s_ (self : t) ?timeout_s () : float =
  match timeout_s with
  | Some t -> t
  | None -> self.default_timeout_s

let call (self : t) ~timer ~(oc : #Io.Out.t Lock.t) ?(headers = []) ?timeout_s
    rpc req : _ Fut.t =
  let timeout_s = get_timeout_s_ self ?timeout_s () in
  let initial_handler = mk_unary_handler self ~timer ~oc ~timeout_s rpc in
  let handler =
    List.fold_left (apply_middleware rpc) initial_handler
      (Lock.get self.st).middlewares
  in

  let ctx = { Handler.headers; hmap = Hmap.empty } in
  handler (ctx, req) |> Fut.map ~f:snd

let call_client_stream (self : t) ~timer ~(oc : #Io.Out.t Lock.t)
    ?(headers = []) ?timeout_s
    (rpc :
      ( 'req,
        Service.Value_mode.stream,
        _,
        Service.Value_mode.unary )
      Pbrt_services.Client.rpc) : 'req Push_stream.t * _ Fut.t =
  (* TODO: can we just avoid that? *)
  let bt = Printexc.get_callstack 5 in

  Option.iter check_timeout_ timeout_s;
  let fut, promise = Fut.make () in

  let in_flight = IF_unary { rpc; promise; bt } in

  let id, meta =
    (* critical section to update internal state *)
    let@ self = Lock.with_lock self.st in
    prepare_query_ self ~rpc ~headers ~in_flight ()
  in

  (* setup timeout, if present *)
  Option.iter
    (fun timeout_s ->
      Timer.run_after_s timer timeout_s (fun () -> handle_timeout self id))
    timeout_s;

  (* now write call to [oc] in as small a critical section as possible *)
  let () =
    let@ enc = with_pbrt_enc_ self in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_empty ~config:self.config ~encoding:self.encoding ~enc oc meta
      ();
    oc#flush ()
  in

  let send_item item : unit =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "bin-rpc.client.send-client-stream-item"
    in
    let meta = Meta.make_meta ~id ~kind:Client_stream_item ~headers () in

    let@ enc = with_pbrt_enc_ self in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_req ~config:self.config ~encoding:self.encoding ~enc oc rpc
      meta item;
    oc#flush ()
  in

  let send_close () : unit =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "bin-rpc.client.send-client-stream-close"
    in
    let meta = Meta.make_meta ~id ~kind:Client_stream_close ~headers () in

    let@ enc = with_pbrt_enc_ self in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_empty ~config:self.config ~encoding:self.encoding ~enc oc meta
      ();
    oc#flush ()
  in

  let stream = { Push_stream.push = send_item; close = send_close } in
  let fut = fut |> Fut.map ~f:snd in
  stream, fut

let call_server_stream (self : t) ~timer ~(oc : #Io.Out.t Lock.t)
    ?(headers = []) ?timeout_s
    (rpc :
      ( 'req,
        Service.Value_mode.unary,
        _,
        Service.Value_mode.stream )
      Pbrt_services.Client.rpc) ~init ~on_item ~on_close req : _ Fut.t =
  (* TODO: can we just avoid that? *)
  let bt = Printexc.get_callstack 5 in
  let timeout_s = get_timeout_s_ self ?timeout_s () in
  check_timeout_ timeout_s;

  let fut, promise = Fut.make () in

  let ctx = { Handler.hmap = Hmap.empty; headers } in
  let state = init (ctx, ()) in
  let in_flight = IF_stream { rpc; state; on_item; on_close; bt; promise } in

  let id, meta =
    (* critical section to update internal state *)
    let@ self = Lock.with_lock self.st in
    prepare_query_ self ~rpc ~headers ~in_flight ()
  in

  Timer.run_after_s timer timeout_s (fun () -> handle_timeout self id);

  send_request_ self ~oc ~meta ~rpc req;
  fut |> Fut.map ~f:snd
