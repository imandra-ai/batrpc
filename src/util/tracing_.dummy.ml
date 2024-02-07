type data =
  [ `Bool of bool
  | `Float of float
  | `Int of int
  | `None
  | `String of string
  ]

let enabled () = false
let dummy_span = 0L
let enter_span ~__FILE__:_ ~__LINE__:_ ?data:_ _name = dummy_span
let exit_span = ignore
let with_span ~__FILE__:_ ~__LINE__:_ ?data:_ _name f = f dummy_span
let set_thread_name _ = ()
