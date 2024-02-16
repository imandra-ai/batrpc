(** TCP client. *)

open Util_

type t = Rpc_conn.t

val connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:Middleware.Client.t list ->
  ?services:Server_state.handler Service.Server.t list ->
  ?encoding:Encoding.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  Unix.sockaddr ->
  t Error.result

val close_and_join : t -> unit
val close_without_joining : t -> unit

val with_connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:Middleware.Client.t list ->
  ?services:Server_state.handler Service.Server.t list ->
  ?encoding:Encoding.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  Unix.sockaddr ->
  (t -> 'a) ->
  'a
