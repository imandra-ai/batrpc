type data =
  [ `Bool of bool
  | `Float of float
  | `Int of int
  | `None
  | `String of string
  ]

val dummy_span : int64

val enter_span :
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * data) list) ->
  string ->
  int64

val exit_span : int64 -> unit

val with_span :
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * data) list) ->
  string ->
  (int64 -> 'a) ->
  'a

val enabled : unit -> bool
val set_thread_name : string -> unit
