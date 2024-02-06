(** TCP client. *)

open Common_

type t = Rpc_conn.t

val connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:Middleware.Async.t list ->
  ?executor:Executor.t ->
  ?services:Service.Server.t list ->
  ?net_stats:Net_stats.t ->
  timer:Timer.t ->
  Unix.sockaddr ->
  t Error.result

val close_and_join : t -> unit

val close_without_joining : t -> unit
