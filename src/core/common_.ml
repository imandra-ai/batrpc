module Service = Pbrt_services
module Log = (val Logs.src_log (Logs.Src.create "batrpc"))
module Runner = Moonpool.Runner

module Int32_tbl = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal
  let hash = Hashtbl.hash
end)

type header = Meta.header
type headers = header list
