module Service = Pbrt_services
module Log = (val Logs.src_log (Logs.Src.create "batrpc"))
module Fut = Moonpool.Fut
module Runner = Moonpool.Runner

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal
  let hash = Hashtbl.hash
end)

module Str_tbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

type header = Meta.header
type headers = header list
