open Common_

type t =
  | Failure of string
  | Deser_error of string
  | Network_error of string
  | Rpc_error of Meta.error
  | Timeout
  | Other of exn * Printexc.raw_backtrace

let show = function
  | Deser_error e -> spf "deserialization error: %s" e
  | Network_error e -> spf "network error: %s" e
  | Rpc_error err -> Format.asprintf "rpc error: %a" Meta.pp_error err
  | Failure f -> spf "failure: %s" f
  | Timeout -> "timeout"
  | Other (exn, _bt) -> Printexc.to_string exn

let pp out self = Format.pp_print_string out (show self)
