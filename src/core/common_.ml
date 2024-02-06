module Service = Pbrt_services

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal
  let hash = Hashtbl.hash
end)

module Log = (val Logs.src_log (Logs.Src.create "ezrpc"))

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
