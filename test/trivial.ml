[@@@ocaml.warning "-27-30-39"]

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

let rec default_pair 
  ?x:((x:string) = "")
  ?y:((y:string) = "")
  ?artificial_delay_s:((artificial_delay_s:float option) = None)
  () : pair  = {
  x;
  y;
  artificial_delay_s;
}

let rec default_big_string 
  ?msg:((msg:string) = "")
  () : big_string  = {
  msg;
}

let rec default_count 
  ?count:((count:int32) = 0l)
  () : count  = {
  count;
}

let rec default_single_int 
  ?i:((i:int32) = 0l)
  () : single_int  = {
  i;
}

type pair_mutable = {
  mutable x : string;
  mutable y : string;
  mutable artificial_delay_s : float option;
}

let default_pair_mutable () : pair_mutable = {
  x = "";
  y = "";
  artificial_delay_s = None;
}

type big_string_mutable = {
  mutable msg : string;
}

let default_big_string_mutable () : big_string_mutable = {
  msg = "";
}

type count_mutable = {
  mutable count : int32;
}

let default_count_mutable () : count_mutable = {
  count = 0l;
}

type single_int_mutable = {
  mutable i : int32;
}

let default_single_int_mutable () : single_int_mutable = {
  i = 0l;
}


(** {2 Make functions} *)

let rec make_pair 
  ~(x:string)
  ~(y:string)
  ?artificial_delay_s:((artificial_delay_s:float option) = None)
  () : pair  = {
  x;
  y;
  artificial_delay_s;
}

let rec make_big_string 
  ~(msg:string)
  () : big_string  = {
  msg;
}

let rec make_count 
  ~(count:int32)
  () : count  = {
  count;
}

let rec make_single_int 
  ~(i:int32)
  () : single_int  = {
  i;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_pair fmt (v:pair) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "x" Pbrt.Pp.pp_string fmt v.x;
    Pbrt.Pp.pp_record_field ~first:false "y" Pbrt.Pp.pp_string fmt v.y;
    Pbrt.Pp.pp_record_field ~first:false "artificial_delay_s" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.artificial_delay_s;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_big_string fmt (v:big_string) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "msg" Pbrt.Pp.pp_string fmt v.msg;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_count fmt (v:count) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "count" Pbrt.Pp.pp_int32 fmt v.count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_single_int fmt (v:single_int) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "i" Pbrt.Pp.pp_int32 fmt v.i;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_pair (v:pair) encoder = 
  Pbrt.Encoder.string v.x encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.y encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  begin match v.artificial_delay_s with
  | Some x -> 
    Pbrt.Encoder.float_as_bits32 x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits32 encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_big_string (v:big_string) encoder = 
  Pbrt.Encoder.string v.msg encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_count (v:count) encoder = 
  Pbrt.Encoder.int32_as_varint v.count encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  ()

let rec encode_pb_single_int (v:single_int) encoder = 
  Pbrt.Encoder.int32_as_varint v.i encoder;
  Pbrt.Encoder.key 0 Pbrt.Varint encoder; 
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_pair d =
  let v = default_pair_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.x <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(pair), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.y <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(pair), field(2)" pk
    | Some (3, Pbrt.Bits32) -> begin
      v.artificial_delay_s <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(pair), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    x = v.x;
    y = v.y;
    artificial_delay_s = v.artificial_delay_s;
  } : pair)

let rec decode_pb_big_string d =
  let v = default_big_string_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.msg <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(big_string), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    msg = v.msg;
  } : big_string)

let rec decode_pb_count d =
  let v = default_count_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(count), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    count = v.count;
  } : count)

let rec decode_pb_single_int d =
  let v = default_single_int_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (0, Pbrt.Varint) -> begin
      v.i <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (0, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(single_int), field(0)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    i = v.i;
  } : single_int)

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_pair (v:pair) = 
  let assoc = [] in 
  let assoc = ("x", Pbrt_yojson.make_string v.x) :: assoc in
  let assoc = ("y", Pbrt_yojson.make_string v.y) :: assoc in
  let assoc = match v.artificial_delay_s with
    | None -> assoc
    | Some v -> ("artificialDelayS", Pbrt_yojson.make_float v) :: assoc
  in
  `Assoc assoc

let rec encode_json_big_string (v:big_string) = 
  let assoc = [] in 
  let assoc = ("msg", Pbrt_yojson.make_string v.msg) :: assoc in
  `Assoc assoc

let rec encode_json_count (v:count) = 
  let assoc = [] in 
  let assoc = ("count", Pbrt_yojson.make_int (Int32.to_int v.count)) :: assoc in
  `Assoc assoc

let rec encode_json_single_int (v:single_int) = 
  let assoc = [] in 
  let assoc = ("i", Pbrt_yojson.make_int (Int32.to_int v.i)) :: assoc in
  `Assoc assoc

[@@@ocaml.warning "-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_pair d =
  let v = default_pair_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("x", json_value) -> 
      v.x <- Pbrt_yojson.string json_value "pair" "x"
    | ("y", json_value) -> 
      v.y <- Pbrt_yojson.string json_value "pair" "y"
    | ("artificialDelayS", json_value) -> 
      v.artificial_delay_s <- Some (Pbrt_yojson.float json_value "pair" "artificial_delay_s")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    x = v.x;
    y = v.y;
    artificial_delay_s = v.artificial_delay_s;
  } : pair)

let rec decode_json_big_string d =
  let v = default_big_string_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("msg", json_value) -> 
      v.msg <- Pbrt_yojson.string json_value "big_string" "msg"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    msg = v.msg;
  } : big_string)

let rec decode_json_count d =
  let v = default_count_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("count", json_value) -> 
      v.count <- Pbrt_yojson.int32 json_value "count" "count"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    count = v.count;
  } : count)

let rec decode_json_single_int d =
  let v = default_single_int_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("i", json_value) -> 
      v.i <- Pbrt_yojson.int32 json_value "single_int" "i"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    i = v.i;
  } : single_int)

module Swapper = struct
  open Pbrt_services.Value_mode
  module Client = struct
    open Pbrt_services
    
    let swap : (pair, unary, pair, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:[]
        ~service_name:"Swapper" ~rpc_name:"swap"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_pair
        ~encode_pb_req:encode_pb_pair
        ~decode_json_res:decode_json_pair
        ~decode_pb_res:decode_pb_pair
        () : (pair, unary, pair, unary) Client.rpc)
    open Pbrt_services
    
    let count_chars : (big_string, unary, count, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:[]
        ~service_name:"Swapper" ~rpc_name:"count_chars"
        ~req_mode:Client.Unary
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_big_string
        ~encode_pb_req:encode_pb_big_string
        ~decode_json_res:decode_json_count
        ~decode_pb_res:decode_pb_count
        () : (big_string, unary, count, unary) Client.rpc)
  end
  
  module Server = struct
    open Pbrt_services
    
    let _rpc_swap : (pair,unary,pair,unary) Server.rpc = 
      (Server.mk_rpc ~name:"swap"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_pair
        ~encode_pb_res:encode_pb_pair
        ~decode_json_req:decode_json_pair
        ~decode_pb_req:decode_pb_pair
        () : _ Server.rpc)
    
    let _rpc_count_chars : (big_string,unary,count,unary) Server.rpc = 
      (Server.mk_rpc ~name:"count_chars"
        ~req_mode:Server.Unary
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_count
        ~encode_pb_res:encode_pb_count
        ~decode_json_req:decode_json_big_string
        ~decode_pb_req:decode_pb_big_string
        () : _ Server.rpc)
    
    let make
      ~swap
      ~count_chars
      () : _ Server.t =
      { Server.
        service_name="Swapper";
        package=[];
        handlers=[
           (swap _rpc_swap);
           (count_chars _rpc_count_chars);
        ];
      }
  end
  
end

module Beancount = struct
  open Pbrt_services.Value_mode
  module Client = struct
    open Pbrt_services
    
    let add_stream : (single_int, stream, count, unary) Client.rpc =
      (Client.mk_rpc 
        ~package:[]
        ~service_name:"Beancount" ~rpc_name:"add_stream"
        ~req_mode:Client.Stream
        ~res_mode:Client.Unary
        ~encode_json_req:encode_json_single_int
        ~encode_pb_req:encode_pb_single_int
        ~decode_json_res:decode_json_count
        ~decode_pb_res:decode_pb_count
        () : (single_int, stream, count, unary) Client.rpc)
    open Pbrt_services
    
    let get_smaller_ints : (count, unary, single_int, stream) Client.rpc =
      (Client.mk_rpc 
        ~package:[]
        ~service_name:"Beancount" ~rpc_name:"get_smaller_ints"
        ~req_mode:Client.Unary
        ~res_mode:Client.Stream
        ~encode_json_req:encode_json_count
        ~encode_pb_req:encode_pb_count
        ~decode_json_res:decode_json_single_int
        ~decode_pb_res:decode_pb_single_int
        () : (count, unary, single_int, stream) Client.rpc)
  end
  
  module Server = struct
    open Pbrt_services
    
    let _rpc_add_stream : (single_int,stream,count,unary) Server.rpc = 
      (Server.mk_rpc ~name:"add_stream"
        ~req_mode:Server.Stream
        ~res_mode:Server.Unary
        ~encode_json_res:encode_json_count
        ~encode_pb_res:encode_pb_count
        ~decode_json_req:decode_json_single_int
        ~decode_pb_req:decode_pb_single_int
        () : _ Server.rpc)
    
    let _rpc_get_smaller_ints : (count,unary,single_int,stream) Server.rpc = 
      (Server.mk_rpc ~name:"get_smaller_ints"
        ~req_mode:Server.Unary
        ~res_mode:Server.Stream
        ~encode_json_res:encode_json_single_int
        ~encode_pb_res:encode_pb_single_int
        ~decode_json_req:decode_json_count
        ~decode_pb_req:decode_pb_count
        () : _ Server.rpc)
    
    let make
      ~add_stream
      ~get_smaller_ints
      () : _ Server.t =
      { Server.
        service_name="Beancount";
        package=[];
        handlers=[
           (add_stream _rpc_add_stream);
           (get_smaller_ints _rpc_get_smaller_ints);
        ];
      }
  end
  
end
