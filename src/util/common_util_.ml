module Trace = Trace_core

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal
  let hash = Hashtbl.hash
end)

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
