(** Basic server for a single client, on an existing connection. *)

type 'a with_ctx = Handler.ctx * 'a
type t

val create :
  ?buf_pool:Buf_pool.t ->
  ?active:Switch.t ->
  ?state:State.t ->
  ?config:Config.t ->
  encoding:Encoding.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  ic:#Io.In.t ->
  oc:#Io.Out.t ->
  unit ->
  t

type handler = State.handler

val close : t -> unit
(** [close rpc] closes the connection. Thread-safe. *)

val runner : t -> Moonpool.Runner.t
(** The background runner for tasks *)

val active : t -> Switch.t option

val state : t -> State.t
(** State for the server end of this connection *)

val add_service : t -> handler Service.Server.t -> unit
(** Add some server-side service to this connection *)

val on_close : t -> unit Fut.t
(** Future to wait for this to close *)

val run : t -> unit
(** [run rpc] runs the server loops on the current thread, blocking
    it, until [close rpc] is called. *)
