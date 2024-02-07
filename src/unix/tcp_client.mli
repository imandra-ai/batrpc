(** TCP client. *)

open Util_

type t = Rpc_conn.t

val connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:Middleware.t list ->
  ?runner:Runner.t ->
  ?services:Server_state.handler Service.Server.t list ->
  timer:Timer.t ->
  Unix.sockaddr ->
  t Error.result

val close_and_join : t -> unit
val close_without_joining : t -> unit

val with_connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:Middleware.t list ->
  ?runner:Runner.t ->
  ?services:Server_state.handler Service.Server.t list ->
  timer:Timer.t ->
  Unix.sockaddr ->
  (t -> 'a) ->
  'a
