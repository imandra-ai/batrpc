open Common_

open struct
  let string_of_error = function
    | Pbrt_yojson.E.Unexpected_json_type (record_name, field_name) ->
      Printf.sprintf "Unexpected json type (record name:%s, field_name:%s)"
        record_name field_name
    | Pbrt_yojson.E.Malformed_variant variant_name ->
      Printf.sprintf "Malformed variant (variant name: %s)" variant_name

  let unwrap_body_size = function
    | Some i -> i
    | None -> Error.raise_err (Error.Deser_error "missing body_size")

  let decode_json_ decode (str : string) =
    match Yojson.Basic.from_string str with
    | j ->
      (try decode j
       with Pbrt_yojson.E.Failure err ->
         let msg = spf "could not decode json: %s" @@ string_of_error err in
         Error.raise_err (Error.Deser_error msg))
    | exception _ -> Error.raise_err (Error.Deser_error "invalid json")

  let read_line_exn_ (ic : #Io.In.t) : string =
    match Io.In.input_line ic with
    | Some s -> s
    | None -> Error.raise_err (Error.Network_error "Could not read next line")
end

(** Size of body above which we apply zlib compression. *)
let compression_threshold = 2 * 1024

let read_meta_b ~buf_pool (ic : #Io.In.t) : Meta.meta option =
  let size_buf = Bytes.create 2 in
  match Io.In.really_input ic size_buf 0 2 with
  | exception End_of_file -> None
  | () ->
    let size = Bytes.get_int16_le size_buf 0 in
    let meta =
      let@ buf = Buf_pool.with_buf buf_pool size in
      Io.In.really_input ic buf 0 size;
      let dec = Pbrt.Decoder.of_subbytes buf 0 size in
      Meta.decode_pb_meta dec
    in

    Some meta

let read_meta_j (ic : #Io.In.t) : Meta.meta option =
  match Io.In.input_line ic with
  | None -> None
  | Some j -> Some (decode_json_ Meta.decode_json_meta j)

let read_meta ~buf_pool ic ~encoding : _ option =
  match encoding with
  | Encoding.Binary -> read_meta_b ~buf_pool ic
  | Encoding.Json -> read_meta_j ic

let read_with_dec_ ~buf_pool ic ~(meta : Meta.meta) ~what ~f_dec =
  let body_size = meta.body_size |> unwrap_body_size |> Int32.to_int in
  let@ buf = Buf_pool.with_buf buf_pool body_size in
  Io.In.really_input ic buf 0 body_size;

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

let read_with_dec_j_ (ic : #Io.In.t) ~what ~f_dec =
  try
    let line = read_line_exn_ ic in
    decode_json_ f_dec line
  with Error.E err -> Error.(failf ~ctx:err "Reading body of %s failed" what)

let read_body_req ~buf_pool (ic : #Io.In.t) ~encoding ~(meta : Meta.meta)
    (rpc : _ Service.Server.rpc) =
  let@ () =
    Error.guardf (fun k ->
        k "Batrpc: reading the request for method %S" rpc.name)
  in
  assert (meta.kind = Meta.Request || meta.kind = Meta.Client_stream_item);
  match encoding with
  | Encoding.Binary ->
    read_with_dec_ ic ~buf_pool ~meta ~what:"request" ~f_dec:rpc.decode_pb_req
  | Encoding.Json ->
    read_with_dec_j_ ~what:"request" ic ~f_dec:rpc.decode_json_req

let read_body_res ~buf_pool (ic : #Io.In.t) ~encoding ~(meta : Meta.meta)
    (rpc : _ Service.Client.rpc) =
  let@ () =
    Error.guardf (fun k ->
        k "Batrpc: reading the response for method %S" rpc.rpc_name)
  in
  assert (meta.kind = Meta.Response || meta.kind = Meta.Server_stream_item);
  match encoding with
  | Encoding.Binary ->
    read_with_dec_ ic ~buf_pool ~meta ~what:"response" ~f_dec:rpc.decode_pb_res
  | Encoding.Json ->
    read_with_dec_j_ ic ~what:"response" ~f_dec:rpc.decode_json_res

let read_error ~buf_pool (ic : #Io.In.t) ~encoding ~(meta : Meta.meta) :
    Meta.error =
  let@ () =
    Error.guardf (fun k -> k "Batrpc: reading the error for call %ld" meta.id)
  in
  assert (meta.kind = Meta.Error);
  match encoding with
  | Encoding.Binary ->
    read_with_dec_ ic ~buf_pool ~meta ~what:"error" ~f_dec:Meta.decode_pb_error
  | Encoding.Json ->
    read_with_dec_j_ ic ~what:"error" ~f_dec:Meta.decode_json_error

let read_and_discard ~buf_pool ic ~encoding ~(meta : Meta.meta) : unit =
  match encoding with
  | Encoding.Binary ->
    let body_size = meta.body_size |> unwrap_body_size |> Int32.to_int in
    let@ buf = Buf_pool.with_buf buf_pool body_size in
    Io.In.really_input ic buf 0 body_size
  | Encoding.Json -> ignore (read_line_exn_ ic : string)

let read_empty ~buf_pool (ic : #Io.In.t) ~encoding ~(meta : Meta.meta) =
  let@ () = Error.guards "Batrpc: reading an Empty message" in
  match encoding with
  | Encoding.Binary ->
    read_with_dec_ ic ~buf_pool ~meta ~what:"empty" ~f_dec:Meta.decode_pb_empty
  | Encoding.Json -> ignore (read_line_exn_ ic : string)

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

let write_meta_b ~enc oc meta : unit =
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

let write_meta_j_ oc meta : unit =
  let j = Meta.encode_json_meta meta |> Yojson.Basic.to_string in
  Io.Out.output_line oc j

let write_with_b_ ?buf_pool ?enc (oc : #Io.Out.t) ~(meta : Meta.meta) ~f_enc x :
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
    { meta with Meta.body_compression; body_size = Some body_size }
  in
  write_meta_b ~enc oc meta;
  oc#output body_str 0 (Bytes.length body_str);
  ()

let write_with_j_ (oc : #Io.Out.t) ~(meta : Meta.meta) ~f_enc x : unit =
  (* send meta *)
  let meta : Meta.meta =
    { meta with Meta.body_compression = None; body_size = None }
  in
  write_meta_j_ oc meta;
  let j = f_enc x |> Yojson.Basic.to_string in
  Io.Out.output_line oc j

let write_req ?buf_pool ?enc (oc : #Io.Out.t) ~encoding
    (rpc : _ Service.Client.rpc) meta req : unit =
  match encoding with
  | Encoding.Binary ->
    write_with_b_ ?buf_pool ?enc oc ~meta ~f_enc:rpc.encode_pb_req req
  | Encoding.Json -> write_with_j_ oc ~meta ~f_enc:rpc.encode_json_req req

let write_error ?buf_pool ?enc (oc : #Io.Out.t) ~encoding meta err : unit =
  match encoding with
  | Encoding.Binary ->
    write_with_b_ ?buf_pool ?enc oc ~meta ~f_enc:Meta.encode_pb_error err
  | Encoding.Json -> write_with_j_ oc ~meta ~f_enc:Meta.encode_json_error err

let write_empty ?buf_pool ?enc (oc : #Io.Out.t) ~encoding meta () : unit =
  match encoding with
  | Encoding.Binary ->
    write_with_b_ ?buf_pool ?enc oc ~meta ~f_enc:Meta.encode_pb_empty ()
  | Encoding.Json -> write_with_j_ oc ~meta ~f_enc:Meta.encode_json_empty ()

let write_res ?buf_pool ?enc (oc : #Io.Out.t) ~encoding
    (rpc : _ Service.Server.rpc) meta res : unit =
  match encoding with
  | Encoding.Binary ->
    write_with_b_ ?buf_pool ?enc oc ~meta ~f_enc:rpc.encode_pb_res res
  | Encoding.Json -> write_with_j_ oc ~meta ~f_enc:rpc.encode_json_res res
