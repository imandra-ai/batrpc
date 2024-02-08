open Common_

(** Size of body above which we apply zlib compression. *)
let compression_threshold = 2 * 1024

let read_meta ~buf_pool (ic : Io.In.t) : Meta.meta option =
  let size_buf = Bytes.create 2 in
  match ic#really_input size_buf 0 2 with
  | exception End_of_file -> None
  | () ->
    let size = Bytes.get_int16_le size_buf 0 in
    let meta =
      let@ buf = Buf_pool.with_buf buf_pool size in
      ic#really_input buf 0 size;
      let dec = Pbrt.Decoder.of_subbytes buf 0 size in
      Meta.decode_pb_meta dec
    in

    Some meta

let read_with_dec_ ~buf_pool ic ~(meta : Meta.meta) ~what ~f_dec =
  let body_size = meta.body_size |> Int32.to_int in
  let@ buf = Buf_pool.with_buf buf_pool body_size in
  ic#really_input buf 0 body_size;

  (* decompress if needed *)
  let buf, body_size =
    match meta.body_compression with
    | Some Meta.Compression_deflate ->
      let body = Util_zlib.decompress_slice buf 0 body_size in
      body, Bytes.length body
    | None | Some Meta.Compression_none -> buf, body_size
  in

  let dec = Pbrt.Decoder.of_subbytes buf 0 body_size in
  try f_dec dec
  with Pbrt.Decoder.Failure err ->
    let err = Pbrt.Decoder.error_to_string err in
    let ctx = Error.(mk @@ Deser_error err) in
    Error.(failf ~ctx "Reading body of %s failed" what)

let read_body_req ~buf_pool (ic : Io.In.t) ~(meta : Meta.meta)
    (rpc : _ Service.Server.rpc) =
  let@ () =
    Error.guardf (fun k ->
        k "Batrpc: reading the request for method %S" rpc.name)
  in
  assert (meta.kind = Meta.Request || meta.kind = Meta.Client_stream_item);
  read_with_dec_ ic ~buf_pool ~meta ~what:"request" ~f_dec:rpc.decode_pb_req

let read_body_res ~buf_pool (ic : Io.In.t) ~(meta : Meta.meta)
    (rpc : _ Service.Client.rpc) =
  let@ () =
    Error.guardf (fun k ->
        k "Batrpc: reading the response for method %S" rpc.rpc_name)
  in
  assert (meta.kind = Meta.Response || meta.kind = Meta.Server_stream_item);
  read_with_dec_ ic ~buf_pool ~meta ~what:"response" ~f_dec:rpc.decode_pb_res

let read_error ~buf_pool (ic : Io.In.t) ~(meta : Meta.meta) : Meta.error =
  let@ () =
    Error.guardf (fun k -> k "Batrpc: reading the error for call %ld" meta.id)
  in
  assert (meta.kind = Meta.Error);
  read_with_dec_ ic ~buf_pool ~meta ~what:"error" ~f_dec:Meta.decode_pb_error

let read_and_discard ~buf_pool ic ~(meta : Meta.meta) : unit =
  let body_size = meta.body_size |> Int32.to_int in
  let@ buf = Buf_pool.with_buf buf_pool body_size in
  ic#really_input buf 0 body_size;
  ()

let read_empty ~buf_pool (ic : Io.In.t) ~(meta : Meta.meta) =
  let@ () = Error.guards "Batrpc: reading an Empty message" in
  read_with_dec_ ic ~buf_pool ~meta ~what:"empty" ~f_dec:Meta.decode_pb_empty

(** Obtain an encoder or reuse [enc]. The encoder will be empty. *)
let with_pbrt_encoder_ ?buf_pool ?enc () f =
  match enc, buf_pool with
  | Some enc, _ ->
    Pbrt.Encoder.clear enc;
    f enc
  | None, Some buf_pool ->
    let@ enc = Buf_pool.with_enc buf_pool in
    Pbrt.Encoder.clear enc;
    f enc
  | None, None -> f (Pbrt.Encoder.create ())

let write_meta_ ~enc (oc : Io.Out.t) (meta : Meta.meta) : unit =
  Pbrt.Encoder.clear enc;
  Meta.encode_pb_meta meta enc;
  (* NOTE: sadly we can't just access the inner buffer, so we need a copy here. *)
  let buf = Pbrt.Encoder.to_bytes enc in

  (* size of [m] has to fit in a [u16] *)
  if Bytes.length buf >= 1 lsl 16 then (
    let msg =
      spf "Batrpc: cannot send message with Metadata of size %d (max size=%d)."
        (Bytes.length buf)
        ((1 lsl 16) - 1)
    in
    Error.raise_err (Network_error msg)
  );

  (* Trace.messagef (fun k -> k "write meta: size=%d" (Bytes.length buf)); *)

  (* write framing *)
  let buf2 = Bytes.create 2 in
  Bytes.set_int16_le buf2 0 (Bytes.length buf);
  oc#output buf2 0 2;

  (* write meta *)
  oc#output buf 0 (Bytes.length buf)

let write_with_ ?buf_pool ?enc (oc : Io.Out.t) ~(meta : Meta.meta) ~f_enc x :
    unit =
  let@ enc = with_pbrt_encoder_ ?buf_pool ?enc () in

  let body_str =
    f_enc x enc;
    Pbrt.Encoder.to_bytes enc
  in

  let body_str, body_compression =
    if Bytes.length body_str > compression_threshold then
      let@ _sp =
        Tracing_.with_span ~__FILE__ ~__LINE__ "framing.compress-body"
      in
      Util_zlib.compress body_str, Some Meta.Compression_deflate
    else
      body_str, None
  in

  (* send meta *)
  let meta : Meta.meta =
    let body_size = Int32.of_int (Bytes.length body_str) in
    { meta with Meta.body_compression; body_size }
  in
  write_meta_ ~enc oc meta;
  oc#output body_str 0 (Bytes.length body_str)

let write_req ?buf_pool ?enc (oc : Io.Out.t) (rpc : _ Service.Client.rpc) meta
    req : unit =
  write_with_ ?buf_pool ?enc oc ~meta ~f_enc:rpc.encode_pb_req req

let write_error ?buf_pool ?enc (oc : Io.Out.t) meta err : unit =
  write_with_ ?buf_pool ?enc oc ~meta ~f_enc:Meta.encode_pb_error err

let write_empty ?buf_pool ?enc (oc : Io.Out.t) meta () : unit =
  write_with_ ?buf_pool ?enc oc ~meta ~f_enc:Meta.encode_pb_empty ()

let write_res ?buf_pool ?enc (oc : Io.Out.t) (rpc : _ Service.Server.rpc) meta
    res : unit =
  write_with_ ?buf_pool ?enc oc ~meta ~f_enc:rpc.encode_pb_res res
