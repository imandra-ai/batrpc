
(** Code for meta.proto *)

(* generated from "meta.proto", do not edit *)



(** {2 Types} *)

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
  body_size : int32;
  kind : kind;
  meth : string option;
  headers : header list;
  body_compression : compression option;
}

type error = {
  msg : string;
}

type empty = unit


(** {2 Basic values} *)

val default_kind : unit -> kind
(** [default_kind ()] is the default value for type [kind] *)

val default_header : 
  ?key:string ->
  ?value:string ->
  unit ->
  header
(** [default_header ()] is the default value for type [header] *)

val default_compression : unit -> compression
(** [default_compression ()] is the default value for type [compression] *)

val default_meta : 
  ?id:int32 ->
  ?body_size:int32 ->
  ?kind:kind ->
  ?meth:string option ->
  ?headers:header list ->
  ?body_compression:compression option ->
  unit ->
  meta
(** [default_meta ()] is the default value for type [meta] *)

val default_error : 
  ?msg:string ->
  unit ->
  error
(** [default_error ()] is the default value for type [error] *)

val default_empty : unit
(** [default_empty ()] is the default value for type [empty] *)


(** {2 Make functions} *)


val make_header : 
  key:string ->
  value:string ->
  unit ->
  header
(** [make_header … ()] is a builder for type [header] *)


val make_meta : 
  id:int32 ->
  body_size:int32 ->
  kind:kind ->
  ?meth:string option ->
  headers:header list ->
  ?body_compression:compression option ->
  unit ->
  meta
(** [make_meta … ()] is a builder for type [meta] *)

val make_error : 
  msg:string ->
  unit ->
  error
(** [make_error … ()] is a builder for type [error] *)



(** {2 Formatters} *)

val pp_kind : Format.formatter -> kind -> unit 
(** [pp_kind v] formats v *)

val pp_header : Format.formatter -> header -> unit 
(** [pp_header v] formats v *)

val pp_compression : Format.formatter -> compression -> unit 
(** [pp_compression v] formats v *)

val pp_meta : Format.formatter -> meta -> unit 
(** [pp_meta v] formats v *)

val pp_error : Format.formatter -> error -> unit 
(** [pp_error v] formats v *)

val pp_empty : Format.formatter -> empty -> unit 
(** [pp_empty v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_kind : kind -> Pbrt.Encoder.t -> unit
(** [encode_pb_kind v encoder] encodes [v] with the given [encoder] *)

val encode_pb_header : header -> Pbrt.Encoder.t -> unit
(** [encode_pb_header v encoder] encodes [v] with the given [encoder] *)

val encode_pb_compression : compression -> Pbrt.Encoder.t -> unit
(** [encode_pb_compression v encoder] encodes [v] with the given [encoder] *)

val encode_pb_meta : meta -> Pbrt.Encoder.t -> unit
(** [encode_pb_meta v encoder] encodes [v] with the given [encoder] *)

val encode_pb_error : error -> Pbrt.Encoder.t -> unit
(** [encode_pb_error v encoder] encodes [v] with the given [encoder] *)

val encode_pb_empty : empty -> Pbrt.Encoder.t -> unit
(** [encode_pb_empty v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_kind : Pbrt.Decoder.t -> kind
(** [decode_pb_kind decoder] decodes a [kind] binary value from [decoder] *)

val decode_pb_header : Pbrt.Decoder.t -> header
(** [decode_pb_header decoder] decodes a [header] binary value from [decoder] *)

val decode_pb_compression : Pbrt.Decoder.t -> compression
(** [decode_pb_compression decoder] decodes a [compression] binary value from [decoder] *)

val decode_pb_meta : Pbrt.Decoder.t -> meta
(** [decode_pb_meta decoder] decodes a [meta] binary value from [decoder] *)

val decode_pb_error : Pbrt.Decoder.t -> error
(** [decode_pb_error decoder] decodes a [error] binary value from [decoder] *)

val decode_pb_empty : Pbrt.Decoder.t -> empty
(** [decode_pb_empty decoder] decodes a [empty] binary value from [decoder] *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_kind : kind -> Yojson.Basic.t
(** [encode_json_kind v encoder] encodes [v] to to json *)

val encode_json_header : header -> Yojson.Basic.t
(** [encode_json_header v encoder] encodes [v] to to json *)

val encode_json_compression : compression -> Yojson.Basic.t
(** [encode_json_compression v encoder] encodes [v] to to json *)

val encode_json_meta : meta -> Yojson.Basic.t
(** [encode_json_meta v encoder] encodes [v] to to json *)

val encode_json_error : error -> Yojson.Basic.t
(** [encode_json_error v encoder] encodes [v] to to json *)

val encode_json_empty : empty -> Yojson.Basic.t
(** [encode_json_empty v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_kind : Yojson.Basic.t -> kind
(** [decode_json_kind decoder] decodes a [kind] value from [decoder] *)

val decode_json_header : Yojson.Basic.t -> header
(** [decode_json_header decoder] decodes a [header] value from [decoder] *)

val decode_json_compression : Yojson.Basic.t -> compression
(** [decode_json_compression decoder] decodes a [compression] value from [decoder] *)

val decode_json_meta : Yojson.Basic.t -> meta
(** [decode_json_meta decoder] decodes a [meta] value from [decoder] *)

val decode_json_error : Yojson.Basic.t -> error
(** [decode_json_error decoder] decodes a [error] value from [decoder] *)

val decode_json_empty : Yojson.Basic.t -> empty
(** [decode_json_empty decoder] decodes a [empty] value from [decoder] *)


(** {2 Services} *)
