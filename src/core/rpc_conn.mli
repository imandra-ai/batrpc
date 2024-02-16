(** RPC over a given connection (pair of channels).

    This is a stateful connection between two client/server
    instances, exchanging messages over a bidirectional stream of bytes.

    Each side can act both as a client and server.
*)

open Common_

type t

val create :
  ?buf_pool:Buf_pool.t ->
  ?active:Switch.t ->
  encoding:Encoding.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  ic:#Io.In.bufferized_t ->
  oc:#Io.Out.t ->
  unit ->
  t

type handler = Server_state.handler

val close_and_join : t -> unit
(** [close_and_join rpc] closes the connection and waits for the
    background thread to finish *)

val close_without_joining : t -> unit
(** [close_without_joining rpc] closes the connection
    but immediately returns. *)

val runner : t -> Moonpool.Runner.t
val active : t -> Switch.t option

val server_state : t -> Server_state.t
(** State for the server end of this connection *)

val client_state : t -> Client_state.t
(** State for the client end of this connection *)

val add_service : t -> handler Service.Server.t -> unit
(** Add some server-side service to this connection *)

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
  init:(unit -> 'state) ->
  on_item:('state -> 'item -> unit) ->
  on_close:('state -> 'res) ->
  'req ->
  'res Fut.t
