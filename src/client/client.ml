open Common_

exception Closed

type 'a with_ctx = Handler.ctx * 'a
type lifecycle_event = Lifecycle.event [@@deriving show]
type lifecycle_state = Lifecycle.state [@@deriving show]

type t = {
  active: Switch.t option;
  timer: Timer.t;
  st: State.t;
  lst: lifecycle_state Atomic.t;
  ic: Io.In.t;  (** Input stream. Only read by the background worker. *)
  oc: Io.Out.t Lock.t;  (** Output stream, shared between many tasks *)
  on_event: lifecycle_event Observer.t;
  encoding: Encoding.t;
  framing_config: Framing.config;
  on_close_promise: unit Fut.promise;
  on_close: unit Fut.t;
  mutable background_worker: Thread.t option;
      (** background thread to receive messages from the other side. *)
}

let[@inline] lifecycle_state (self : t) = Atomic.get self.lst
let[@inline] on_close self = self.on_close
let[@inline] state self = self.st
let[@inline] active self = self.active
let[@inline] on_event self = self.on_event

(** Change the current state using [f] *)
let transition_state (self : t) f =
  let res, old_st, new_st = Atomic_util.modify_with self.lst f in
  if not (Lifecycle.equal_state old_st new_st) then
    Observer.emit self.on_event (Lifecycle.Set_state new_st);
  res

(** run [f()] and, if it fails, log a warning. *)
let with_logging_error_as_warning_ what f =
  try f ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = Error.of_exn ~bt ~kind:Errors.network exn in
    Log.warn (fun k -> k "Rpc_conn: %s:@ %a" what Error.pp err)

let close_real_ ~join_bg ~send_close self : unit =
  let@ () = with_logging_error_as_warning_ "failure when closing" in
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rpc.client.close" in

  let () =
    let@ oc = Lock.with_lock self.oc in

    (* send a "close" message. *)
    if send_close then (
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
  let action =
    transition_state self (function
      | Other_side_disconnected -> `close_silently, Disconnected
      | Connected -> `close_and_notify, Disconnected
      | Disconnected -> `do_nothing, Disconnected)
  in
  match action with
  | `do_nothing -> ()
  | `close_silently -> close_real_ ~join_bg ~send_close:false self
  | `close_and_notify -> close_real_ ~join_bg ~send_close:true self

(** Close connection, from the another thread. *)
let close_and_join self = close_ ~join_bg:true self

(** Close from inside the background worker. Can't join itself. *)
let close_without_joining (self : t) : unit = close_ ~join_bg:false self

let wait_block_close self : unit = Fut.wait_block_exn self.on_close

let handle_close (self : t) : unit =
  Log.info (fun k -> k "RPC.client: remote side closed the connection");
  let must_close =
    transition_state self (function
      | Other_side_disconnected | Disconnected -> false, Disconnected
      | Connected -> true, Other_side_disconnected)
  in
  if must_close then close_without_joining self

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
  while
    Lifecycle.can_receive (lifecycle_state self) && is_on_or_absent self.active
  do
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
    ?(on_event = Observer.create ()) ?(track_io_events = false) ~encoding ~timer
    ~(ic : #Io.In.t) ~(oc : #Io.Out.t) () : t =
  let framing_config =
    Framing.make_config ~buf_pool ~use_zlib:config.use_zlib ()
  in
  let st =
    State.create ~framing_config ~default_timeout_s:config.default_timeout_s
      ~encoding ()
  in

  let ic = (ic :> Io.In.t) in
  let oc = (oc :> Io.Out.t) in

  let ic, oc =
    if track_io_events then (
      (* reflect IO operations in [on_event] *)
      let ic =
        new Io.In.instrument ic ~on_read:(fun bs i len ->
            Observer.emit on_event (Lifecycle.Read (bs, i, len)))
      in
      let oc =
        new Io.Out.instrument oc ~on_write:(fun bs i len ->
            Observer.emit on_event (Lifecycle.Write (bs, i, len)))
      in
      ic, oc
    ) else
      ic, oc
  in

  let on_close, on_close_promise = Fut.make () in
  let self =
    {
      st;
      active;
      lst = Atomic.make Lifecycle.Connected;
      encoding;
      on_event;
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
  if not (Lifecycle.can_send @@ lifecycle_state self) then raise Closed;
  State.call self.st ~timer:self.timer ?headers ?timeout_s ~oc:self.oc rpc req

let call_client_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) : _ Push_stream.t * _ Fut.t =
  if not (Lifecycle.can_send @@ lifecycle_state self) then raise Closed;
  State.call_client_stream self.st ~timer:self.timer ?headers ?timeout_s
    ~oc:self.oc rpc

let call_server_stream (self : t) ?headers ?timeout_s
    (rpc : _ Pbrt_services.Client.rpc) ~init ~on_item ~on_close req : _ Fut.t =
  if not (Lifecycle.can_send @@ lifecycle_state self) then raise Closed;
  State.call_server_stream self.st ~timer:self.timer ?headers ?timeout_s
    ~oc:self.oc rpc ~init ~on_item ~on_close req
