(** Server side for a given connection. *)

open Common_

type t
(** Server side for a given connection to a client *)

val pp : Format.formatter -> t -> unit
val show : t -> string

type handler = Server_handler.t

val mk_handler :
  ( 'req,
    Service.Value_mode.unary,
    'res,
    Service.Value_mode.unary )
  Service.Server.rpc ->
  ('req -> 'res Fut.t) ->
  handler
(** Make a simple request/reply handler *)

val mk_handler_full :
  ( 'req,
    Service.Value_mode.unary,
    'res,
    Service.Value_mode.unary )
  Service.Server.rpc ->
  (header list -> 'req -> (header list * 'res) Fut.t) ->
  handler
(** Make a full request/reply handler, with headers *)

val mk_client_stream_handler :
  ( 'req,
    Service.Value_mode.stream,
    'res,
    Service.Value_mode.unary )
  Service.Server.rpc ->
  init:(unit -> 'state) ->
  on_item:('state -> 'req -> unit) ->
  on_close:('state -> 'res) ->
  unit ->
  handler
(** Make a handler for client-stream requests. The handler is
 a state machine that reacts to new stream elements and returns
 a response upon stream closure. *)

val mk_server_stream_handler :
  ( 'req,
    Service.Value_mode.unary,
    'res,
    Service.Value_mode.stream )
  Service.Server.rpc ->
  ('req -> 'res Push_stream.t -> unit) ->
  handler
(** Make a handler for server-stream. The handler can push values
   back to the client. The request ends when the stream
   is closed by the handler. *)

val create :
  ?middlewares:Middleware.Server.t list ->
  services:handler Service.Server.t list ->
  unit ->
  t

val add_middleware : t -> Middleware.Server.t -> unit
val add_service : t -> handler Service.Server.t -> unit
val list_services : t -> handler Service.Server.t list
val find_meth : t -> string -> (string * handler) option

val handle_request :
  t ->
  runner:Runner.t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:#Io.In.bufferized_t ->
  oc:#Io.Out.t Lock.t ->
  unit ->
  unit
(** We have read, from [ic], the metadata for a new message
    of type [Meta.Request].
    Handle this by running (in the current thread) the
    corresponding handler and reply to it.
  @param runner where to spawn the handler for this request? *)

val handle_stream_item :
  t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:#Io.In.bufferized_t ->
  oc:#Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)

val handle_stream_close :
  t ->
  buf_pool:Buf_pool.t ->
  meta:Meta.meta ->
  ic:#Io.In.bufferized_t ->
  oc:#Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)
