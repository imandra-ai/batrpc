(** Basic client that doesn't have a server component *)

type 'a with_ctx = Handler.ctx * 'a

type t
(** The client connection *)

type lifecycle_event = Lifecycle.event [@@deriving show]
type lifecycle_state = Lifecycle.state [@@deriving show]

val create :
  ?buf_pool:Buf_pool.t ->
  ?active:Switch.t ->
  ?config:Config.t ->
  ?on_event:lifecycle_event Observer.t ->
  ?track_io_events:bool ->
  encoding:Encoding.t ->
  timer:Timer.t ->
  ic:#Io.In.t ->
  oc:#Io.Out.t ->
  unit ->
  t
(** Create a new client.
    @param on_event observer for events
    @param track_io_events if true, instrument [ic] and [oc] to record all
    reads and writes into the [on_event] observer *)

val close_and_join : t -> unit
(** [close_and_join rpc] closes the connection and waits for the
    background thread to finish *)

val close_without_joining : t -> unit
(** [close_without_joining rpc] closes the connection
    but immediately returns. *)

val active : t -> Switch.t option

val state : t -> State.t
(** State for the client end of this connection *)

val on_event : t -> lifecycle_event Observer.t
val lifecycle_state : t -> lifecycle_state

val on_close : t -> unit Fut.t
(** Future to wait for this to close *)

val wait_block_close : t -> unit
(** [wait_block_close rpc] is [Fut.wait_block_exn @@ on_close rpc].
    Be careful with deadlocks. *)

exception Closed

val call :
  t ->
  ?headers:Meta.header list ->
  ?timeout_s:float ->
  ( 'req,
    Service.Value_mode.unary,
    'res,
    Service.Value_mode.unary )
  Service.Client.rpc ->
  'req ->
  'res Fut.t
(** @raise Closed if the connection is closed *)

val call_client_stream :
  t ->
  ?headers:Meta.header list ->
  ?timeout_s:float ->
  ( 'req,
    Service.Value_mode.stream,
    'res,
    Service.Value_mode.unary )
  Service.Client.rpc ->
  'req Push_stream.t * 'res Fut.t

val call_server_stream :
  t ->
  ?headers:Meta.header list ->
  ?timeout_s:float ->
  ( 'req,
    Service.Value_mode.unary,
    'item,
    Service.Value_mode.stream )
  Service.Client.rpc ->
  init:(unit with_ctx -> 'state) ->
  on_item:('state -> 'item -> unit) ->
  on_close:('state -> 'res with_ctx) ->
  'req ->
  'res Fut.t
