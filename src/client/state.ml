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

type t = { st: state Lock.t } [@@unboxed]

let[@inline] remove_from_tbl_ (self : state) id : unit =
  Int32_tbl.remove self.in_flight id

let add_middleware self m : unit =
  let@ self = Lock.with_lock self.st in
  self.middlewares <- m :: self.middlewares

(** Handle a message of "response" type *)
let handle_response (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~encoding () :
    unit =
  (* Trace.message "bin-rpc.client.handle-res"; *)
  assert (meta.kind = Meta.Response);

  let@ self = Lock.with_lock self.st in
  match Int32_tbl.find_opt self.in_flight meta.id with
  | None ->
    Log.err (fun k ->
        k "client: received response: no request had id %ld" meta.id);
    Framing.read_and_discard ~buf_pool ~meta ~encoding ic
  | Some (IF_unary { rpc; promise; _ }) ->
    remove_from_tbl_ self meta.id;
    let res = Framing.read_body_res ~buf_pool ic ~encoding rpc ~meta in
    let ctx = { Handler.headers = meta.headers; hmap = Hmap.empty } in
    Fut.fulfill_idempotent promise (Ok (ctx, res))
  | Some (IF_stream _) ->
    remove_from_tbl_ self meta.id;
    Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
    Log.err (fun k ->
        k "client: received response: expected stream item for id %ld" meta.id)

let handle_stream_item (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~encoding ()
    : unit =
  (* Trace.message "bin-rpc.client.handle-stream-item"; *)
  assert (meta.kind = Meta.Server_stream_item);

  let todo =
    let@ self = Lock.with_lock self.st in
    match Int32_tbl.find_opt self.in_flight meta.id with
    | None ->
      Log.err (fun k ->
          k "client: received stream item: no request had id %ld" meta.id);
      Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
      None
    | Some (IF_unary _) ->
      remove_from_tbl_ self meta.id;
      Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
      Log.err (fun k ->
          k "client: received stream item: expected response for id %ld" meta.id);
      None
    | Some (IF_stream { rpc; state; on_item; _ }) ->
      let res = Framing.read_body_res ~buf_pool ~encoding ic rpc ~meta in
      Some (fun () -> on_item state res)
  in

  match todo with
  | None -> ()
  | Some f -> f ()

let handle_stream_close (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~encoding
    () : unit =
  (* Trace.message "bin-rpc.client.handle-stream-close"; *)
  assert (meta.kind = Meta.Server_stream_close);

  let todo =
    let@ self = Lock.with_lock self.st in
    match Int32_tbl.find_opt self.in_flight meta.id with
    | None ->
      Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
      Log.err (fun k ->
          k "client: received stream-close: no request had id %ld" meta.id);
      None
    | Some (IF_unary _) ->
      remove_from_tbl_ self meta.id;
      Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
      Log.err (fun k ->
          k "client: received stream item: expected response for id %ld" meta.id);
      None
    | Some (IF_stream { on_close; state; promise; _ }) ->
      remove_from_tbl_ self meta.id;
      Framing.read_empty ~buf_pool ~meta ~encoding ic;
      Some
        (fun () ->
          let res = on_close state in
          Fut.fulfill_idempotent promise (Ok res))
  in

  match todo with
  | None -> ()
  | Some f -> f ()

(** Handle a message of "error" type *)
let handle_error (self : t) ~buf_pool ~(meta : Meta.meta) ~ic ~encoding () :
    unit =
  let@ self = Lock.with_lock self.st in
  match Int32_tbl.find_opt self.in_flight meta.id with
  | None ->
    Framing.read_and_discard ~buf_pool ~meta ~encoding ic;
    Log.err (fun k -> k "client: received error: no request had id %ld" meta.id)
  | Some (IF_unary { promise; bt; _ }) ->
    remove_from_tbl_ self meta.id;
    let err = Framing.read_error ~buf_pool ic ~encoding ~meta in
    let err = Error.mk_error ~kind:Errors.remote err.msg in
    Log.err (fun k ->
        k "client: received error for id %ld:@ %a" meta.id Error.pp err);
    Fut.fulfill_idempotent promise (Error (Error.E err, bt))
  | Some (IF_stream { bt; promise; _ }) ->
    remove_from_tbl_ self meta.id;
    let err = Framing.read_error ~buf_pool ~encoding ic ~meta in
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

let create ?(middlewares = []) () : t =
  {
    st =
      Lock.create { counter = 0; middlewares; in_flight = Int32_tbl.create 8 };
  }

(** Call [f] with a protobuf encoder *)
let with_pbrt_enc_ ?buf_pool () f =
  match buf_pool with
  | None -> f (Pbrt.Encoder.create ())
  | Some pool -> Buf_pool.with_enc pool f

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
let send_request_ ?buf_pool ~oc ~encoding ~meta ~rpc req : unit =
  let@ enc = with_pbrt_enc_ ?buf_pool () in
  Pbrt.Encoder.clear enc;

  let@ oc = Lock.with_lock oc in
  Framing.write_req ~enc ~encoding oc rpc meta req;
  oc#flush ()

let mk_unary_handler (self : t) ?buf_pool ~timer ~oc ~encoding ~timeout_s rpc :
    _ Handler.t =
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

  send_request_ ?buf_pool ~rpc ~oc ~encoding ~meta req;
  fut

let default_timeout_s_ : float = 30.

let send_heartbeat ~buf_pool ~oc ~encoding () : unit =
  let meta = Meta.make_meta ~id:0l ~kind:Meta.Heartbeat ~headers:[] () in
  let@ oc = Lock.with_lock oc in
  Framing.write_empty ~buf_pool ~encoding oc meta ();
  oc#flush ()

let call (self : t) ?buf_pool ~timer ~(oc : #Io.Out.t Lock.t) ~encoding
    ?(headers = []) ?(timeout_s = default_timeout_s_) rpc req : _ Fut.t =
  let initial_handler =
    mk_unary_handler self ?buf_pool ~timer ~oc ~encoding ~timeout_s rpc
  in
  let handler =
    List.fold_left (apply_middleware rpc) initial_handler
      (Lock.get self.st).middlewares
  in

  let ctx = { Handler.headers; hmap = Hmap.empty } in
  handler (ctx, req) |> Fut.map ~f:snd

let call_client_stream (self : t) ?buf_pool ~timer ~(oc : #Io.Out.t Lock.t)
    ~encoding ?(headers = []) ?timeout_s
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
    let@ enc = with_pbrt_enc_ ?buf_pool () in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_empty ~enc ~encoding oc meta ();
    oc#flush ()
  in

  let send_item item : unit =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "bin-rpc.client.send-client-stream-item"
    in
    let meta = Meta.make_meta ~id ~kind:Client_stream_item ~headers () in

    let@ enc = with_pbrt_enc_ ?buf_pool () in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_req ~enc oc ~encoding rpc meta item;
    oc#flush ()
  in

  let send_close () : unit =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        "bin-rpc.client.send-client-stream-close"
    in
    let meta = Meta.make_meta ~id ~kind:Client_stream_close ~headers () in

    let@ enc = with_pbrt_enc_ ?buf_pool () in
    Pbrt.Encoder.clear enc;

    let@ oc = Lock.with_lock oc in
    Framing.write_empty ~enc ~encoding oc meta ();
    oc#flush ()
  in

  let stream = { Push_stream.push = send_item; close = send_close } in
  let fut = fut |> Fut.map ~f:snd in
  stream, fut

let call_server_stream (self : t) ?buf_pool ~timer ~(oc : #Io.Out.t Lock.t)
    ~encoding ?(headers = []) ?(timeout_s = default_timeout_s_)
    (rpc :
      ( 'req,
        Service.Value_mode.unary,
        _,
        Service.Value_mode.stream )
      Pbrt_services.Client.rpc) ~init ~on_item ~on_close req : _ Fut.t =
  (* TODO: can we just avoid that? *)
  let bt = Printexc.get_callstack 5 in
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

  send_request_ ?buf_pool ~oc ~encoding ~meta ~rpc req;
  fut |> Fut.map ~f:snd
