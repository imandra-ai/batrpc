[@@@ocaml.warning "-27-30-39"]

type kind =
  | Invalid 
  | Request 
  | Response 
  | Error 
  | Client_stream_item 
  | Client_stream_close 
  | Server_stream_item 
  | Server_stream_close 
  | Heartbeat 
  | Close 

type header = {
  key : string;
  value : string;
}

type compression =
  | Compression_none 
  | Compression_deflate 

type meta = {
  id : int32;
  body_size : int32 option;
  kind : kind;
  meth : string option;
  headers : header list;
  body_compression : compression option;
}

type error = {
  msg : string;
}

type empty = unit

let rec default_kind () = (Invalid:kind)

let rec default_header 
  ?key:((key:string) = "")
  ?value:((value:string) = "")
  () : header  = {
  key;
  value;
}

let rec default_compression () = (Compression_none:compression)

let rec default_meta 
  ?id:((id:int32) = 0l)
  ?body_size:((body_size:int32 option) = None)
  ?kind:((kind:kind) = default_kind ())
  ?meth:((meth:string option) = None)
  ?headers:((headers:header list) = [])
  ?body_compression:((body_compression:compression option) = None)
  () : meta  = {
  id;
  body_size;
  kind;
  meth;
  headers;
  body_compression;
}

let rec default_error 
  ?msg:((msg:string) = "")
  () : error  = {
  msg;
}

let rec default_empty = ()

type header_mutable = {
  mutable key : string;
  mutable value : string;
}

let default_header_mutable () : header_mutable = {
  key = "";
  value = "";
}

type meta_mutable = {
  mutable id : int32;
  mutable body_size : int32 option;
  mutable kind : kind;
  mutable meth : string option;
  mutable headers : header list;
  mutable body_compression : compression option;
}

let default_meta_mutable () : meta_mutable = {
  id = 0l;
  body_size = None;
  kind = default_kind ();
  meth = None;
  headers = [];
  body_compression = None;
}

type error_mutable = {
  mutable msg : string;
}

let default_error_mutable () : error_mutable = {
  msg = "";
}


(** {2 Make functions} *)


let rec make_header 
  ~(key:string)
  ~(value:string)
  () : header  = {
  key;
  value;
}


let rec make_meta 
  ~(id:int32)
  ?body_size:((body_size:int32 option) = None)
  ~(kind:kind)
  ?meth:((meth:string option) = None)
  ~(headers:header list)
  ?body_compression:((body_compression:compression option) = None)
  () : meta  = {
  id;
  body_size;
  kind;
  meth;
  headers;
  body_compression;
}

let rec make_error 
  ~(msg:string)
  () : error  = {
  msg;
}


[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_kind fmt (v:kind) =
  match v with
  | Invalid -> Format.fprintf fmt "Invalid"
  | Request -> Format.fprintf fmt "Request"
  | Response -> Format.fprintf fmt "Response"
  | Error -> Format.fprintf fmt "Error"
  | Client_stream_item -> Format.fprintf fmt "Client_stream_item"
  | Client_stream_close -> Format.fprintf fmt "Client_stream_close"
  | Server_stream_item -> Format.fprintf fmt "Server_stream_item"
  | Server_stream_close -> Format.fprintf fmt "Server_stream_close"
  | Heartbeat -> Format.fprintf fmt "Heartbeat"
  | Close -> Format.fprintf fmt "Close"

let rec pp_header fmt (v:header) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "key" Pbrt.Pp.pp_string fmt v.key;
    Pbrt.Pp.pp_record_field ~first:false "value" Pbrt.Pp.pp_string fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_compression fmt (v:compression) =
  match v with
  | Compression_none -> Format.fprintf fmt "Compression_none"
  | Compression_deflate -> Format.fprintf fmt "Compression_deflate"

let rec pp_meta fmt (v:meta) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "id" Pbrt.Pp.pp_int32 fmt v.id;
    Pbrt.Pp.pp_record_field ~first:false "body_size" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int32) fmt v.body_size;
    Pbrt.Pp.pp_record_field ~first:false "kind" pp_kind fmt v.kind;
    Pbrt.Pp.pp_record_field ~first:false "meth" (Pbrt.Pp.pp_option Pbrt.Pp.pp_string) fmt v.meth;
    Pbrt.Pp.pp_record_field ~first:false "headers" (Pbrt.Pp.pp_list pp_header) fmt v.headers;
    Pbrt.Pp.pp_record_field ~first:false "body_compression" (Pbrt.Pp.pp_option pp_compression) fmt v.body_compression;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_error fmt (v:error) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "msg" Pbrt.Pp.pp_string fmt v.msg;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_empty fmt (v:empty) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_unit fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_kind (v:kind) encoder =
  match v with
  | Invalid -> Pbrt.Encoder.int_as_varint (0) encoder
  | Request -> Pbrt.Encoder.int_as_varint 1 encoder
  | Response -> Pbrt.Encoder.int_as_varint 2 encoder
  | Error -> Pbrt.Encoder.int_as_varint 3 encoder
  | Client_stream_item -> Pbrt.Encoder.int_as_varint 4 encoder
  | Client_stream_close -> Pbrt.Encoder.int_as_varint 5 encoder
  | Server_stream_item -> Pbrt.Encoder.int_as_varint 6 encoder
  | Server_stream_close -> Pbrt.Encoder.int_as_varint 7 encoder
  | Heartbeat -> Pbrt.Encoder.int_as_varint 30 encoder
  | Close -> Pbrt.Encoder.int_as_varint 400 encoder

let rec encode_pb_header (v:header) encoder = 
  Pbrt.Encoder.string v.key encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.value encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_compression (v:compression) encoder =
  match v with
  | Compression_none -> Pbrt.Encoder.int_as_varint (0) encoder
  | Compression_deflate -> Pbrt.Encoder.int_as_varint 1 encoder

let rec encode_pb_meta (v:meta) encoder = 
  Pbrt.Encoder.int32_as_varint v.id encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  begin match v.body_size with
  | Some x -> 
    Pbrt.Encoder.int32_as_varint x encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  | None -> ();
  end;
  encode_pb_kind v.kind encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  begin match v.meth with
  | Some x -> 
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_header x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.headers encoder;
  begin match v.body_compression with
  | Some x -> 
    encode_pb_compression x encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_error (v:error) encoder = 
  Pbrt.Encoder.string v.msg encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_empty (v:empty) encoder = 
()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_kind d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Invalid:kind)
  | 1 -> (Request:kind)
  | 2 -> (Response:kind)
  | 3 -> (Error:kind)
  | 4 -> (Client_stream_item:kind)
  | 5 -> (Client_stream_close:kind)
  | 6 -> (Server_stream_item:kind)
  | 7 -> (Server_stream_close:kind)
  | 30 -> (Heartbeat:kind)
  | 400 -> (Close:kind)
  | _ -> Pbrt.Decoder.malformed_variant "kind"

let rec decode_pb_header d =
  let v = default_header_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(header), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(header), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    key = v.key;
    value = v.value;
  } : header)

let rec decode_pb_compression d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Compression_none:compression)
  | 1 -> (Compression_deflate:compression)
  | _ -> Pbrt.Decoder.malformed_variant "compression"

let rec decode_pb_meta d =
  let v = default_meta_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.headers <- List.rev v.headers;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.body_size <- Some (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.kind <- decode_pb_kind d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.meth <- Some (Pbrt.Decoder.string d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.headers <- (decode_pb_header (Pbrt.Decoder.nested d)) :: v.headers;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.body_compression <- Some (decode_pb_compression d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(meta), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    id = v.id;
    body_size = v.body_size;
    kind = v.kind;
    meth = v.meth;
    headers = v.headers;
    body_compression = v.body_compression;
  } : meta)

let rec decode_pb_error d =
  let v = default_error_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.msg <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(error), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    msg = v.msg;
  } : error)

let rec decode_pb_empty d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(empty)" pk

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_kind (v:kind) = 
  match v with
  | Invalid -> `String "Invalid"
  | Request -> `String "Request"
  | Response -> `String "Response"
  | Error -> `String "Error"
  | Client_stream_item -> `String "Client_stream_item"
  | Client_stream_close -> `String "Client_stream_close"
  | Server_stream_item -> `String "Server_stream_item"
  | Server_stream_close -> `String "Server_stream_close"
  | Heartbeat -> `String "Heartbeat"
  | Close -> `String "Close"

let rec encode_json_header (v:header) = 
  let assoc = [] in 
  let assoc = ("key", Pbrt_yojson.make_string v.key) :: assoc in
  let assoc = ("value", Pbrt_yojson.make_string v.value) :: assoc in
  `Assoc assoc

let rec encode_json_compression (v:compression) = 
  match v with
  | Compression_none -> `String "Compression_none"
  | Compression_deflate -> `String "Compression_deflate"

let rec encode_json_meta (v:meta) = 
  let assoc = [] in 
  let assoc = ("id", Pbrt_yojson.make_int (Int32.to_int v.id)) :: assoc in
  let assoc = match v.body_size with
    | None -> assoc
    | Some v -> ("bodySize", Pbrt_yojson.make_int (Int32.to_int v)) :: assoc
  in
  let assoc = ("kind", encode_json_kind v.kind) :: assoc in
  let assoc = match v.meth with
    | None -> assoc
    | Some v -> ("meth", Pbrt_yojson.make_string v) :: assoc
  in
  let assoc =
    let l = v.headers |> List.map encode_json_header in
    ("headers", `List l) :: assoc 
  in
  let assoc = match v.body_compression with
    | None -> assoc
    | Some v -> ("bodyCompression", encode_json_compression v) :: assoc
  in
  `Assoc assoc

let rec encode_json_error (v:error) = 
  let assoc = [] in 
  let assoc = ("msg", Pbrt_yojson.make_string v.msg) :: assoc in
  `Assoc assoc

let rec encode_json_empty (v:empty) = 
Pbrt_yojson.make_unit v

[@@@ocaml.warning "-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_kind json =
  match json with
  | `String "Invalid" -> (Invalid : kind)
  | `String "Request" -> (Request : kind)
  | `String "Response" -> (Response : kind)
  | `String "Error" -> (Error : kind)
  | `String "Client_stream_item" -> (Client_stream_item : kind)
  | `String "Client_stream_close" -> (Client_stream_close : kind)
  | `String "Server_stream_item" -> (Server_stream_item : kind)
  | `String "Server_stream_close" -> (Server_stream_close : kind)
  | `String "Heartbeat" -> (Heartbeat : kind)
  | `String "Close" -> (Close : kind)
  | _ -> Pbrt_yojson.E.malformed_variant "kind"

let rec decode_json_header d =
  let v = default_header_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("key", json_value) -> 
      v.key <- Pbrt_yojson.string json_value "header" "key"
    | ("value", json_value) -> 
      v.value <- Pbrt_yojson.string json_value "header" "value"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    key = v.key;
    value = v.value;
  } : header)

let rec decode_json_compression json =
  match json with
  | `String "Compression_none" -> (Compression_none : compression)
  | `String "Compression_deflate" -> (Compression_deflate : compression)
  | _ -> Pbrt_yojson.E.malformed_variant "compression"

let rec decode_json_meta d =
  let v = default_meta_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("id", json_value) -> 
      v.id <- Pbrt_yojson.int32 json_value "meta" "id"
    | ("bodySize", json_value) -> 
      v.body_size <- Some (Pbrt_yojson.int32 json_value "meta" "body_size")
    | ("kind", json_value) -> 
      v.kind <- (decode_json_kind json_value)
    | ("meth", json_value) -> 
      v.meth <- Some (Pbrt_yojson.string json_value "meta" "meth")
    | ("headers", `List l) -> begin
      v.headers <- List.map (function
        | json_value -> (decode_json_header json_value)
      ) l;
    end
    | ("bodyCompression", json_value) -> 
      v.body_compression <- Some ((decode_json_compression json_value))
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    id = v.id;
    body_size = v.body_size;
    kind = v.kind;
    meth = v.meth;
    headers = v.headers;
    body_compression = v.body_compression;
  } : meta)

let rec decode_json_error d =
  let v = default_error_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("msg", json_value) -> 
      v.msg <- Pbrt_yojson.string json_value "error" "msg"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    msg = v.msg;
  } : error)

let rec decode_json_empty d =
Pbrt_yojson.unit d "empty" "empty record"
