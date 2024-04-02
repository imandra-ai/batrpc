(** Wire encoding *)

type t =
  | Binary
  | Json

let show = function
  | Binary -> "binary"
  | Json -> "json"

let pp = Fmt.of_to_string show

(** The encoding as a u32_le, for negociation *)
let to_int32_le = function
  | Binary -> Bytes.get_int32_le (Bytes.unsafe_of_string "BATB") 0
  | Json -> Bytes.get_int32_le (Bytes.unsafe_of_string "BATJ") 0

(** Find encoding from the initial 4 bytes of a connection *)
let of_int32_le i =
  let bs = Bytes.create 4 in
  Bytes.set_int32_le bs 0 i;
  match Bytes.unsafe_to_string bs with
  | "BATB" -> Some Binary
  | "BATJ" -> Some Json
  | _ -> None

let write_to_oc (oc : #Io.Out.t) (self : t) : unit =
  let bs4 = Bytes.create 4 in
  Bytes.set_int32_le bs4 0 (to_int32_le self);
  oc#output bs4 0 4;
  oc#flush ()

let read_from_ic (ic : #Io.In.t) : t =
  let magic_number =
    let bs4 = Bytes.create 4 in
    Io.In.really_input ic bs4 0 4;
    Bytes.get_int32_le bs4 0
  in

  match of_int32_le magic_number with
  | Some e -> e
  | None ->
    Error.failf ~kind:Errors.deser "Batrpc: read invalid magic number %ld"
      magic_number
