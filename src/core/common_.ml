module Service = Pbrt_services
module Runner = Moonpool.Runner
module Log = (val Logs.src_log (Logs.Src.create "batrpc"))
module Slice = Iostream.Slice

type header = Meta.header
type headers = header list
