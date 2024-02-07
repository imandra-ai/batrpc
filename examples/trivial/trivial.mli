
(** Code for trivial.proto *)

(* generated from "trivial.proto", do not edit *)



(** {2 Types} *)

type pair = {
  x : string;
  y : string;
  artificial_delay_s : float option;
}

type big_string = {
  msg : string;
}

type count = {
  count : int32;
}

type single_int = {
  i : int32;
}


(** {2 Basic values} *)

val default_pair : 
  ?x:string ->
  ?y:string ->
  ?artificial_delay_s:float option ->
  unit ->
  pair
(** [default_pair ()] is the default value for type [pair] *)

val default_big_string : 
  ?msg:string ->
  unit ->
  big_string
(** [default_big_string ()] is the default value for type [big_string] *)

val default_count : 
  ?count:int32 ->
  unit ->
  count
(** [default_count ()] is the default value for type [count] *)

val default_single_int : 
  ?i:int32 ->
  unit ->
  single_int
(** [default_single_int ()] is the default value for type [single_int] *)


(** {2 Make functions} *)

val make_pair : 
  x:string ->
  y:string ->
  ?artificial_delay_s:float option ->
  unit ->
  pair
(** [make_pair … ()] is a builder for type [pair] *)

val make_big_string : 
  msg:string ->
  unit ->
  big_string
(** [make_big_string … ()] is a builder for type [big_string] *)

val make_count : 
  count:int32 ->
  unit ->
  count
(** [make_count … ()] is a builder for type [count] *)

val make_single_int : 
  i:int32 ->
  unit ->
  single_int
(** [make_single_int … ()] is a builder for type [single_int] *)


(** {2 Formatters} *)

val pp_pair : Format.formatter -> pair -> unit 
(** [pp_pair v] formats v *)

val pp_big_string : Format.formatter -> big_string -> unit 
(** [pp_big_string v] formats v *)

val pp_count : Format.formatter -> count -> unit 
(** [pp_count v] formats v *)

val pp_single_int : Format.formatter -> single_int -> unit 
(** [pp_single_int v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_pair : pair -> Pbrt.Encoder.t -> unit
(** [encode_pb_pair v encoder] encodes [v] with the given [encoder] *)

val encode_pb_big_string : big_string -> Pbrt.Encoder.t -> unit
(** [encode_pb_big_string v encoder] encodes [v] with the given [encoder] *)

val encode_pb_count : count -> Pbrt.Encoder.t -> unit
(** [encode_pb_count v encoder] encodes [v] with the given [encoder] *)

val encode_pb_single_int : single_int -> Pbrt.Encoder.t -> unit
(** [encode_pb_single_int v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_pair : Pbrt.Decoder.t -> pair
(** [decode_pb_pair decoder] decodes a [pair] binary value from [decoder] *)

val decode_pb_big_string : Pbrt.Decoder.t -> big_string
(** [decode_pb_big_string decoder] decodes a [big_string] binary value from [decoder] *)

val decode_pb_count : Pbrt.Decoder.t -> count
(** [decode_pb_count decoder] decodes a [count] binary value from [decoder] *)

val decode_pb_single_int : Pbrt.Decoder.t -> single_int
(** [decode_pb_single_int decoder] decodes a [single_int] binary value from [decoder] *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_pair : pair -> Yojson.Basic.t
(** [encode_json_pair v encoder] encodes [v] to to json *)

val encode_json_big_string : big_string -> Yojson.Basic.t
(** [encode_json_big_string v encoder] encodes [v] to to json *)

val encode_json_count : count -> Yojson.Basic.t
(** [encode_json_count v encoder] encodes [v] to to json *)

val encode_json_single_int : single_int -> Yojson.Basic.t
(** [encode_json_single_int v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_pair : Yojson.Basic.t -> pair
(** [decode_json_pair decoder] decodes a [pair] value from [decoder] *)

val decode_json_big_string : Yojson.Basic.t -> big_string
(** [decode_json_big_string decoder] decodes a [big_string] value from [decoder] *)

val decode_json_count : Yojson.Basic.t -> count
(** [decode_json_count decoder] decodes a [count] value from [decoder] *)

val decode_json_single_int : Yojson.Basic.t -> single_int
(** [decode_json_single_int decoder] decodes a [single_int] value from [decoder] *)


(** {2 Services} *)

(** Swapper service *)
module Swapper : sig
  open Pbrt_services
  open Pbrt_services.Value_mode
  
  module Client : sig
    
    val swap : (pair, unary, pair, unary) Client.rpc
    
    val count_chars : (big_string, unary, count, unary) Client.rpc
  end
  
  module Server : sig
    (** Produce a server implementation from handlers *)
    val make : 
      swap:((pair, unary, pair, unary) Server.rpc -> 'handler) ->
      count_chars:((big_string, unary, count, unary) Server.rpc -> 'handler) ->
      unit -> 'handler Pbrt_services.Server.t
  end
end
