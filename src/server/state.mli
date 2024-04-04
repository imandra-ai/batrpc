(** Server side for a given connection. *)

type t
(** Server side for a given connection to a client *)

val pp : Format.formatter -> t -> unit
val show : t -> string

type handler = Server_handler.t
type 'a with_ctx = Handler.ctx * 'a

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
  ('req with_ctx -> 'res with_ctx Fut.t) ->
  handler
(** Make a full request/reply handler, with headers *)

val mk_client_stream_handler :
  ( 'req,
    Service.Value_mode.stream,
    'res,
    Service.Value_mode.unary )
  Service.Server.rpc ->
  init:(unit with_ctx -> 'state) ->
  on_item:('state -> 'req -> unit) ->
  on_close:('state -> 'res with_ctx) ->
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
  ('req with_ctx -> 'res Push_stream.t -> unit) ->
  handler
(** Make a handler for server-stream. The handler can push values
   back to the client. The request ends when the stream
   is closed by the handler. *)

val create :
  ?middlewares:Middleware.t list ->
  services:handler Service.Server.t list ->
  framing_config:Framing.config ->
  unit ->
  t

val add_middleware : t -> Middleware.t -> unit
val add_service : t -> handler Service.Server.t -> unit
val list_services : t -> handler Service.Server.t list
val find_meth : t -> string -> (string * handler) option

val send_heartbeat :
  t -> encoding:Encoding.t -> oc:#Io.Out.t Lock.t -> unit -> unit
(** Send a heartbeat message *)

val handle_request :
  t ->
  encoding:Encoding.t ->
  runner:Runner.t ->
  meta:Meta.meta ->
  ic:#Io.In.t ->
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
  encoding:Encoding.t ->
  meta:Meta.meta ->
  ic:#Io.In.t ->
  oc:#Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)

val handle_stream_close :
  t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  ic:#Io.In.t ->
  oc:#Io.Out.t Lock.t ->
  unit ->
  unit
(** Handle stream close, in the current thread. We must preserve the order of
    processing. *)
