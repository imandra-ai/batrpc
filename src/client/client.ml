open Common_

exception Closed

type 'a with_ctx = Handler.ctx * 'a

type t = {
  active: Switch.t option;
  is_open: bool Atomic.t;
  other_side_did_close: bool Atomic.t;
  timer: Timer.t;
  st: State.t;
  ic: Io.In.t;  (** Input stream. Only read by the background worker. *)
  oc: Io.Out.t Lock.t;  (** Output stream, shared between many tasks *)
  encoding: Encoding.t;
  framing_config: Framing.config;
  on_close_promise: unit Fut.promise;
  on_close: unit Fut.t;
  mutable background_worker: Thread.t option;
      (** background thread to receive messages from the other side. *)
}

let on_close self = self.on_close
let state self = self.st
let active self = self.active

(** run [f()] and, if it fails, log a warning. *)
let with_logging_error_as_warning_ what f =
  try f ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:Errors.network exn in
    Log.warn (fun k -> k "Rpc_conn: %s:@ %a" what Error.pp err)

let close_real_ ~join_bg self : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.client.close" in

  let () =
    let@ oc = Lock.with_lock self.oc in

    (* send a "close" message. *)
    if not (Atomic.get self.other_side_did_close) then (
      let@ () =
        with_logging_error_as_warning_ "could not send 'close' message"
      in
      Log.debug (fun k -> k "send 'close' message");
      Framing.write_empty oc ~config:self.framing_config ~encoding:self.encoding
        (Meta.make_meta ~id:0l ~kind:Meta.Close ~headers:[] ())
        ();
      oc#flush ()
    );
    oc#close ()
  in
  self.ic#close ();

  let join_thread_ th =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.client.join" in
    Thread.join th
  in

  (* now stop worker *)
  if join_bg then Option.iter join_thread_ self.background_worker;
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

let wait_block_close self : unit = Fut.wait_block_exn self.on_close

let handle_close (self : t) : unit =
  Log.info (fun k -> k "RPC.client: remote side closed the connection");
  Atomic.set self.other_side_did_close true;
  close_without_joining self

open struct
  let is_on_or_absent = function
    | None -> true
    | Some sw -> Switch.is_on sw
end

(** Just read the empty body *)
let handle_heartbeat (self : t) ~meta : unit =
  Framing.read_empty ~config:self.framing_config ~encoding:self.encoding self.ic
    ~meta;
  ()

(** Main loop for the background worker. *)
let background_worker_loop_ (self : t) : unit =
  Trace.set_thread_name "rpc.client.bg";
  let@ () = with_logging_error_as_warning_ "running background loop" in
  while Atomic.get self.is_open && is_on_or_absent self.active do
    match
      (* TODO: use a timeout in the read *)
      Framing.read_meta ~config:self.framing_config ~encoding:self.encoding
        self.ic
    with
    | exception End_of_file ->
      Log.debug (fun k -> k "reached end-of-file");
      handle_close self
    | exception Sys_error msg ->
      Log.warn (fun k ->
          k "RPC client failed to read message: sys error %s" msg);
      handle_close self
    | None -> handle_close self
    | Some meta ->
      Log.debug (fun k -> k "rpc.client: read message %a" Meta.pp_meta meta);

      (*Trace.messagef (fun k ->
          k "rpc conn.bg: read msg %a" Meta.pp_kind meta.kind);*)

      (* examine the incoming message *)
      (match meta.kind with
      | Close ->
        Log.debug (fun k -> k "got `close` message");
        handle_close self
      | Response -> State.handle_response self.st ~meta ~ic:self.ic ()
      | Error -> State.handle_error self.st ~meta ~ic:self.ic ()
      | Heartbeat -> handle_heartbeat self ~meta
      | Server_stream_item ->
        State.handle_stream_item self.st ~meta ~ic:self.ic ()
      | Server_stream_close ->
        State.handle_stream_close self.st ~meta ~ic:self.ic ()
      | Request | Client_stream_item | Client_stream_close | Invalid ->
        Log.err (fun k ->
            k "client: unexpected message of kind=%a" Meta.pp_kind meta.kind);
        (* cannot continue, we haven't read the body and there is a protocol bug. *)
        close_without_joining self)
  done;
  Log.debug (fun k -> k "rpc-conn bg: exiting")

let send_heartbeat (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.server.send-heartbeat" in

  try State.send_heartbeat self.st ~oc:self.oc ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:Errors.network exn in
    Log.err (fun k -> k "Could not send heartbeat:@ %a" Error.pp err);
    close_ self ~join_bg:false

let create ?(buf_pool = Buf_pool.create ()) ?active ?(config = Config.default)
    ~encoding ~timer ~(ic : #Io.In.t) ~(oc : #Io.Out.t) () : t =
  let framing_config =
    Framing.make_config ~buf_pool ~use_zlib:config.use_zlib ()
  in
  let st =
    State.create ~framing_config ~default_timeout_s:config.default_timeout_s
      ~encoding ()
  in

  let ic = (ic :> Io.In.t) in
  let oc = (oc :> Io.Out.t) in

  let on_close, on_close_promise = Fut.make () in
  let self =
    {
      st;
      active;
      is_open = Atomic.make true;
      other_side_did_close = Atomic.make false;
      encoding;
      framing_config;
      timer;
      ic;
      oc = Lock.create oc;
      on_close;
      on_close_promise;
      background_worker = None;
    }
  in

  self.background_worker <- Some (Thread.create background_worker_loop_ self);

  (* send regular hearbeats *)
  let timer_handle =
    match config.heartbeat_interval_s with
    | None -> Timer.Handle.dummy
    | Some t ->
      Timer.run_every_s' timer ~initial:0.005 t (fun () -> send_heartbeat self)
  in
  Fut.on_result on_close (fun _ -> Timer.cancel timer timer_handle);

  let close_when_active_is_off () = close_without_joining self in
  Option.iter
    (fun active -> Switch.on_turn_off active close_when_active_is_off)
    active;

  self

let call (self : t) ?headers ?timeout_s (rpc : _ Pbrt_services.Client.rpc) req :
    _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  State.call self.st ~timer:self.timer ?headers ?timeout_s ~oc:self.oc rpc req

let call_client_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) : _ Push_stream.t * _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  State.call_client_stream self.st ~timer:self.timer ?headers ?timeout_s
    ~oc:self.oc rpc

let call_server_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) ~init ~on_item ~on_close req : _ Fut.t =
  if not (Atomic.get self.is_open) then raise Closed;
  State.call_server_stream self.st ~timer:self.timer ?headers ?timeout_s
    ~oc:self.oc rpc ~init ~on_item ~on_close req
