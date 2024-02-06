module Service = Pbrt_services
module Trace = Trace_core
module Log = (val Logs.src_log (Logs.Src.create "batrpc"))
module Fut = Moonpool.Fut

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal
  let hash = Hashtbl.hash
end)

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
