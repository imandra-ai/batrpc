(** Server side for a given connection. *)

open Common_

type t [@@deriving show]
(** Server side for a given connection to a client *)

val create :
  ?middlewares:Middleware.Sync.t list ->
  services:Service.Server.t list ->
  unit ->
  t

val add_middleware : t -> Middleware.Sync.t -> unit

val add_service : t -> Service.Server.t -> unit

val list_services : t -> Service.Server.t list

val find_meth : t -> string -> Service.Server.any_rpc option

val handle_request :
  t ->
  executor:Executor.t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:Io.In.t ->
  oc:Io.Out.t Lock.t ->
  unit ->
  unit
(** We have read, from [ic], the metadata for a new message
    of type [Meta.Request].
    Handle this by running (in the current thread) the
    corresponding handler and reply to it.
  @param executor where to spawn the handler for this request? *)

val handle_stream_item :
  t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:Io.In.t ->
  oc:Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)

val handle_stream_close :
  t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:Io.In.t ->
  oc:Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)
