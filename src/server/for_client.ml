open Common_

type 'a with_ctx = Handler.ctx * 'a
type handler = State.handler

type t = {
  active: Switch.t option;
  config: Config.t;
  is_open: bool Atomic.t;
  other_side_did_close: bool Atomic.t;
  buf_pool: Buf_pool.t;
  timer: Timer.t;
  runner: Runner.t;
  encoding: Encoding.t;
  st: State.t;
  ic: Io.In.t;  (** Input stream. Only read by the background worker. *)
  oc: Io.Out.t Lock.t;  (** Output stream, shared between many tasks *)
  on_close_promise: unit Fut.promise;
  on_close: unit Fut.t;
}

let on_close self = self.on_close
let state self = self.st
let add_service self s : unit = State.add_service self.st s
let runner self = self.runner
let active self = self.active

(** run [f()] and, if it fails, log a warning. *)
let with_logging_error_as_warning_ what f =
  try f ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:Errors.network exn in
    Log.warn (fun k -> k "Rpc_conn: %s:@ %a" what Error.pp err)

let close_real_ (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.conn.close" in

  let () =
    let@ oc = Lock.with_lock self.oc in

    (* send a "close" message. *)
    if not (Atomic.get self.other_side_did_close) then (
      let@ () =
        with_logging_error_as_warning_ "could not send 'close' message"
      in
      Log.debug (fun k -> k "send 'close' message");
      Framing.write_empty oc ~encoding:self.encoding
        (Meta.make_meta ~id:0l ~kind:Meta.Close ~headers:[] ())
        ();
      oc#flush ()
    );
    oc#close ()
  in
  self.ic#close ();

  Fut.fulfill_idempotent self.on_close_promise @@ Ok ()

let close self =
  if Atomic.exchange self.is_open false then
    let@ () = with_logging_error_as_warning_ "failure when closing" in
    close_real_ self

let handle_close (self : t) : unit =
  Log.info (fun k -> k "Server: remote side closed the connection");
  Atomic.set self.other_side_did_close true;
  close self

open struct
  let is_on_or_absent = function
    | None -> true
    | Some sw -> Switch.is_on sw
end

let send_heartbeat (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.send-heartbeat" in

  try
    State.send_heartbeat ~buf_pool:self.buf_pool ~encoding:self.encoding
      ~oc:self.oc ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:Errors.network exn in
    Log.err (fun k -> k "Could not send heartbeat:@ %a" Error.pp err);
    close self

(** Just read the empty body *)
let handle_heartbeat (self : t) ~meta : unit =
  Framing.read_empty ~buf_pool:self.buf_pool self.ic ~encoding:self.encoding
    ~meta;
  ()

(** Main loop for the background worker. *)
let run (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.for-client.run" in
  let@ () = with_logging_error_as_warning_ "handling RPC client" in

  let timer_handle =
    match self.config.heartbeat_interval_s with
    | None -> Timer.Handle.dummy
    | Some t ->
      Timer.run_every_s' self.timer ~initial:0.005 t (fun () ->
          send_heartbeat self)
  in
  let@ () =
    Fun.protect ~finally:(fun () -> Timer.cancel self.timer timer_handle)
  in

  while Atomic.get self.is_open && is_on_or_absent self.active do
    match
      Framing.read_meta self.ic ~encoding:self.encoding ~buf_pool:self.buf_pool
    with
    | exception End_of_file -> handle_close self
    | exception Sys_error msg ->
      Log.warn (fun k -> k "Server: failed to read message: sys error %s" msg);
      handle_close self
    | None -> handle_close self
    | Some meta ->
      Log.debug (fun k ->
          k "for-client: new incoming message %a" Meta.pp_kind meta.kind);

      (*Trace.messagef (fun k ->
          k "rpc conn.bg: read msg %a" Meta.pp_kind meta.kind);*)

      (* examine the incoming message *)
      (match meta.kind with
      | Close -> handle_close self
      | Heartbeat -> handle_heartbeat self ~meta
      | Request ->
        State.handle_request self.st ~runner:self.runner ~buf_pool:self.buf_pool
          ~meta ~encoding:self.encoding ~ic:self.ic ~oc:self.oc ()
      | Client_stream_item ->
        State.handle_stream_item self.st ~buf_pool:self.buf_pool ~meta
          ~ic:self.ic ~oc:self.oc ~encoding:self.encoding ()
      | Client_stream_close ->
        State.handle_stream_close self.st ~buf_pool:self.buf_pool ~meta
          ~ic:self.ic ~oc:self.oc ~encoding:self.encoding ()
      | Error | Server_stream_item | Server_stream_close | Response | Invalid ->
        Log.err (fun k ->
            k "client: unexpected message of kind=%a" Meta.pp_kind meta.kind);
        (* cannot continue, we haven't read the body and there is a protocol bug. *)
        close self)
  done;
  Log.debug (fun k -> k "rpc-conn bg: exiting")

let create ?(buf_pool = Buf_pool.create ()) ?active ?state
    ?(config = Config.default) ~encoding ~runner ~timer ~(ic : #Io.In.t)
    ~(oc : #Io.Out.t) () : t =
  let state =
    match state with
    | None -> State.create ~services:[] ()
    | Some st -> st
  in

  let ic = (ic :> Io.In.t) in
  let oc = (oc :> Io.Out.t) in

  let on_close, on_close_promise = Fut.make () in
  let self =
    {
      st = state;
      config;
      encoding;
      active;
      timer;
      runner;
      is_open = Atomic.make true;
      other_side_did_close = Atomic.make false;
      buf_pool;
      ic;
      oc = Lock.create oc;
      on_close;
      on_close_promise;
    }
  in

  let close_when_active_is_off () =
    (* run close asynchronously, to avoid blocking other switch callbacks *)
    Runner.run_async self.runner (fun () -> close self)
  in

  Option.iter
    (fun active -> Switch.on_turn_off active close_when_active_is_off)
    active;

  self
