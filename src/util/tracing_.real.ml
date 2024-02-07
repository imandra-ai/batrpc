module Trace = Trace_core

type data =
  [ `Bool of bool
  | `Float of float
  | `Int of int
  | `None
  | `String of string
  ]

let enabled = Trace.enabled
let dummy_span = Int64.min_int
let set_thread_name = Trace.set_thread_name

let[@inline] enter_span ~__FILE__ ~__LINE__ ?data name : int64 =
  if name = "" then
    dummy_span
  else
    Trace.enter_span ~__FILE__ ~__LINE__ ?data name

let[@inline] exit_span sp = if sp <> dummy_span then Trace.exit_span sp

let[@inline] with_span ~__FILE__ ~__LINE__ ?data name f =
  Trace.with_span ~__FILE__ ~__LINE__ ?data name f
