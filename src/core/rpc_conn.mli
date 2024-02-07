(** RPC over a given connection (pair of channels).

    This is a stateful connection between two client/server
    instances, exchanging messages over a bidirectional stream of bytes.

    Each side can act both as a client and server.
*)

open Common_

type t

val create :
  ?server_state:Server_state.t ->
  ?client_state:Client_state.t ->
  ?buf_pool:Buf_pool.t ->
  ?active:Switch.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  ic:Io.In.t ->
  oc:Io.Out.t ->
  unit ->
  t

type handler = Server_state.handler

val close_and_join : t -> unit
val close_without_joining : t -> unit
val server_state : t -> Server_state.t
val client_state : t -> Client_state.t
val add_service : t -> handler Service.Server.t -> unit
val on_close : t -> unit Fut.t

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
