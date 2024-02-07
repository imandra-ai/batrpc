(** Cancellation switch *)

type t = {
  is_on: unit -> bool;
  on_turn_off: (unit -> unit) -> unit;
  turn_off: unit -> unit;
}

let[@inline] is_on (self : t) : bool = self.is_on ()
let[@inline] is_cancelled self : bool = not (is_on self)
let[@inline] turn_off self : unit = self.turn_off ()

let[@inline] is_on_or_absent (self : t option) : bool =
  match self with
  | None -> true
  | Some s -> is_on s

let[@inline] on_turn_off self f : unit = self.on_turn_off f
