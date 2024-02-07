open Common_

exception Closed

type handler = Server_state.handler

type t = {
  active: Switch.t option;
  is_open: bool Atomic.t;
  other_side_did_close: bool Atomic.t;
  buf_pool: Buf_pool.t;
  runner: Runner.t;
  timer: Timer.t;
  as_server: Server_state.t;
  as_client: Client_state.t;
  ic: Io.In.t;  (** Input stream. Only read by the background worker. *)
  oc: Io.Out.t Lock.t;  (** Output stream, shared between many tasks *)
  on_close_promise: unit Fut.promise;
  on_close: unit Fut.t;
  mutable background_worker: Thread.t option;
      (** background thread to receive messages from the other side. *)
}

let on_close self = self.on_close
let client_state self = self.as_client
let server_state self = self.as_server
let add_service self s : unit = Server_state.add_service self.as_server s

(** run [f()] and, if it fails, log a warning. *)
let with_logging_error_as_warning_ what f =
  try f ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt exn |> Error.wrap (Error.Network_error "") in
    Log.warn (fun k -> k "Rpc_conn: %s:@ %a" what Error.pp err)

let close_real_ ~join_bg self : unit =
  let@ _sp = Tracing_.with_span ~__FILE__ ~__LINE__ "bin-rpc.conn.close" in

  let () =
    let@ oc = Lock.with_lock self.oc in

    (* send a "close" message. *)
    if not (Atomic.get self.other_side_did_close) then (
      let@ () =
        with_logging_error_as_warning_ "could not send 'close' message"
      in
      Framing.write_empty oc
        (Meta.make_meta ~id:0l ~body_size:0l ~kind:Meta.Close ~headers:[] ())
        ();
      oc#flush ()
    );
    oc#close ()
  in
  self.ic#close ();

  (* now stop worker *)
  if join_bg then Option.iter Thread.join self.background_worker;
  self.background_worker <- None;

  Fut.fulfill_idempotent self.on_close_promise @@ Ok ()

let close_ ~join_bg self =
  if Atomic.exchange self.is_open false then
    let@ () = with_logging_error_as_warning_ "failure when closing" in
    close_real_ ~join_bg self

(** Close connection, from the another thread. *)
let close_and_join self = close_ ~join_bg:true self

(** Close from inside the background worker. Can't join itself. *)
let close_without_joining (self : t) : unit = close_ ~join_bg:false self

let handle_close (self : t) : unit =
  Log.info (fun k -> k "Bin_rpc.conn: remote side closed the connection");
  Atomic.set self.other_side_did_close true;
  close_without_joining self

(** Main loop for the background worker. *)
let background_worker (self : t) : unit =
  Tracing_.set_thread_name "rpc-conn.bg";
  let@ () = with_logging_error_as_warning_ "running background loop" in
  while Atomic.get self.is_open && Switch.is_on_or_absent self.active do
    match Framing.read_meta self.ic ~buf_pool:self.buf_pool with
    | exception End_of_file -> handle_close self
    | exception Sys_error msg ->
      Log.warn (fun k -> k "RPC conn failed to read message: sys error %s" msg);
      handle_close self
    | None -> handle_close self
    | Some meta ->
      (*Trace.messagef (fun k ->
          k "rpc conn.bg: read msg %a" Meta.pp_kind meta.kind);*)

      (* examine the incoming message *)
      (match meta.kind with
      | Close -> handle_close self
      | Response ->
        Client_state.handle_response self.as_client ~buf_pool:self.buf_pool
          ~meta ~ic:self.ic ()
      | Error ->
        Client_state.handle_error self.as_client ~buf_pool:self.buf_pool ~meta
          ~ic:self.ic ()
      | Heartbeat -> ()
      | Request ->
        Server_state.handle_request self.as_server ~runner:self.runner
          ~buf_pool:self.buf_pool ~meta ~ic:self.ic ~oc:self.oc ()
      | Client_stream_item ->
        Server_state.handle_stream_item self.as_server ~buf_pool:self.buf_pool
          ~meta ~ic:self.ic ~oc:self.oc ()
      | Client_stream_close ->
        Server_state.handle_stream_close self.as_server ~buf_pool:self.buf_pool
          ~meta ~ic:self.ic ~oc:self.oc ()
      | Server_stream_item ->
        Client_state.handle_stream_item self.as_client ~buf_pool:self.buf_pool
          ~meta ~ic:self.ic ()
      | Server_stream_close ->
        Client_state.handle_stream_close self.as_client ~buf_pool:self.buf_pool
          ~meta ~ic:self.ic ()
      | Invalid ->
        Log.err (fun k ->
            k "client: unexpected message of kind=%a" Meta.pp_kind meta.kind);
        (* cannot continue, we haven't read the body and there is a protocol bug. *)
        close_without_joining self)
  done;
  Log.debug (fun k -> k "rpc-conn bg: exiting")

let create ?(server_state = Server_state.create ~services:[] ())
    ?(client_state = Client_state.create ()) ?(buf_pool = Buf_pool.create ())
    ?active ~runner ~timer ~ic ~oc () : t =
  let on_close, on_close_promise = Fut.make () in
  let self =
    {
      as_server = server_state;
      as_client = client_state;
      active;
      runner;
      is_open = Atomic.make true;
      other_side_did_close = Atomic.make false;
      buf_pool;
      timer;
      ic;
      oc = Lock.create oc;
      on_close;
      on_close_promise;
      background_worker = None;
    }
  in

  self.background_worker <- Some (Thread.create background_worker self);

  let close_when_active_is_off () =
    (* run close asynchronously, to avoid blocking other switch callbacks *)
    Runner.run_async self.runner (fun () -> close_without_joining self)
  in

  Option.iter
    (fun active -> Switch.on_turn_off active close_when_active_is_off)
    active;

  self

let call (self : t) ?headers ?timeout_s (rpc : _ Pbrt_services.Client.rpc) req :
    _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  Client_state.call self.as_client ~timer:self.timer ?headers ?timeout_s
    ~oc:self.oc rpc req

let call_client_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) : _ Push_stream.t * _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  Client_state.call_client_stream self.as_client ~buf_pool:self.buf_pool
    ~timer:self.timer ?headers ?timeout_s ~oc:self.oc rpc

let call_server_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) ~init ~on_item ~on_close req : _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  Client_state.call_server_stream self.as_client ~buf_pool:self.buf_pool
    ~timer:self.timer ?headers ?timeout_s ~oc:self.oc rpc ~init ~on_item
    ~on_close req
