(** Framing.

  This part of the library is concerned with framing, ie. how we
  parse and separate messages on the wire. We work over streams
  of bytes so we need to know where each message starts and ends.

  To do so, each message starts with a [u16] in little endian.
  It indicates how many bytes the following [Meta.meta] message takes.
  The [Meta.meta] message  provides information about the given RPC message
  (id, method, etc.) as well as the size in bytes of the [body], ie.
  the main protobuf object passed as argument (or reply) to the remote
  method.

  Framing also handles compression automatically.
*)

open Common_

val read_meta :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  Meta.meta option

val read_body_req :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  ('req, _, _, _) Service.Server.rpc ->
  'req

val read_body_res :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  (_, _, 'res, _) Service.Client.rpc ->
  'res

val read_and_discard :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  unit
(** Read message body but do not decode it. This is useful if we know we need
    to ignore it (e.g. reply to a request that timed out) but still
    need to remove bytes from the socket. *)

val read_error :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  Meta.error

val read_empty :
  buf_pool:Buf_pool.t ->
  #Io.In.bufferized_t ->
  encoding:Encoding.t ->
  meta:Meta.meta ->
  unit

val write_req :
  ?buf_pool:Buf_pool.t ->
  ?enc:Pbrt.Encoder.t ->
  #Io.Out.bufferized_t ->
  encoding:Encoding.t ->
  ('req, _, _, _) Service.Client.rpc ->
  Meta.meta ->
  'req ->
  unit

val write_error :
  ?buf_pool:Buf_pool.t ->
  ?enc:Pbrt.Encoder.t ->
  #Io.Out.bufferized_t ->
  encoding:Encoding.t ->
  Meta.meta ->
  Meta.error ->
  unit

val write_empty :
  ?buf_pool:Buf_pool.t ->
  ?enc:Pbrt.Encoder.t ->
  #Io.Out.bufferized_t ->
  encoding:Encoding.t ->
  Meta.meta ->
  unit ->
  unit

val write_res :
  ?buf_pool:Buf_pool.t ->
  ?enc:Pbrt.Encoder.t ->
  #Io.Out.bufferized_t ->
  encoding:Encoding.t ->
  (_, _, 'res, _) Service.Server.rpc ->
  Meta.meta ->
  'res ->
  unit
