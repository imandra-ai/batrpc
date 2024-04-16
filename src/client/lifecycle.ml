type json = Yojson.Safe.t

type body =
  | Body : ('a -> json) * 'a -> body
      (** Body for those messages that have it *)

let pp_body out _ = Fmt.string out "<body>"

type state =
  | Connected
  | Other_side_disconnected
  | Disconnected
[@@deriving show { with_path = false }, eq]

let[@inline] can_receive = function
  | Other_side_disconnected (* maybe there's still buffered data *) | Connected
    ->
    true
  | Disconnected -> false

let[@inline] can_send = function
  | Connected -> true
  | Disconnected | Other_side_disconnected -> false

type event =
  | Set_state of state
  | Read of bytes * int * int
      (** Read event. The slice is only valid in the context of this event. *)
  | Write of bytes * int * int
      (** Read event. The slice is only valid in the context of this event. *)
[@@deriving show { with_path = false }]
