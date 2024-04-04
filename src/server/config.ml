type t = {
  heartbeat_interval_s: float option; [@default Some 5.]
  use_zlib: bool; [@default true]
}
[@@deriving make, show { with_path = false }]
(** Config for client *)

let default : t = make ()
