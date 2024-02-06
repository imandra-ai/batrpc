open Common_
module Push_stream = Push_stream

type t

val add_middleware : t -> Middleware.t -> unit
val create : ?middlewares:Middleware.t list -> unit -> t

val handle_response :
  t -> buf_pool:Buf_pool.t -> meta:Meta.meta -> ic:Io.In.t -> unit -> unit

val handle_error :
  t -> buf_pool:Buf_pool.t -> meta:Meta.meta -> ic:Io.In.t -> unit -> unit

val handle_stream_item :
  t -> buf_pool:Buf_pool.t -> meta:Meta.meta -> ic:Io.In.t -> unit -> unit

val handle_stream_close :
  t -> buf_pool:Buf_pool.t -> meta:Meta.meta -> ic:Io.In.t -> unit -> unit

val call :
  t ->
  ?buf_pool:Buf_pool.t ->
  timer:Timer.t ->
  oc:Io.Out.t Lock.t ->
  ?headers:Meta.header list ->
  ?timeout_s:float ->
  ( 'req,
    Service.Value_mode.unary,
    'res,
    Service.Value_mode.unary )
  Service.Client.rpc ->
  'req ->
  'res Fut.t
(** Call a regular RPC method. *)

val call_client_stream :
  t ->
  ?buf_pool:Buf_pool.t ->
  timer:Timer.t ->
  oc:Io.Out.t Lock.t ->
  ?headers:Meta.header list ->
  ?timeout_s:float ->
  ( 'req,
    Service.Value_mode.stream,
    'res,
    Service.Value_mode.unary )
  Service.Client.rpc ->
  'req Push_stream.t * 'res Fut.t
(** Call a RPC method that takes a client stream.
    This returns a pair [stream, res]; you can then push items
  onto the stream using {!Push_stream.push}. When it's done,
  call {!Push_stream.close} and the future will resolve once the
  server has processed it. *)

val call_server_stream :
  t ->
  ?buf_pool:Buf_pool.t ->
  timer:Timer.t ->
  oc:Io.Out.t Lock.t ->
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
(** Call a RPC method that takes a request and returns a stream from
    the server. *)
