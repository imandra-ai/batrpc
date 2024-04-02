(** TCP client. *)

module C = Batrpc_client

type t = C.t

val connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:C.Middleware.t list ->
  ?encoding:Encoding.t ->
  timer:Timer.t ->
  Unix.sockaddr ->
  t Error.result

val close_and_join : t -> unit
val close_without_joining : t -> unit

val with_connect :
  ?active:Switch.t ->
  ?buf_pool:Buf_pool.t ->
  ?middlewares:C.Middleware.t list ->
  ?encoding:Encoding.t ->
  timer:Timer.t ->
  Unix.sockaddr ->
  (t -> 'a) ->
  'a
