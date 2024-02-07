type t = {
  run_after_s: float -> (unit -> unit) -> unit;
  run_every_s: float -> (unit -> unit) -> unit;
  terminate: unit -> unit;
}
(** Timer, to run callbacks in the future *)

exception Stop_timer

(** [run_after_s t f] waits [t] seconds and then runs [f] *)
let[@inline] run_after_s (self : t) delay f = self.run_after_s delay f

(** [run_every ~initial t f] waits [initial] seconds and
    then runs [f()] every [t] seconds.
    [f ()] can raise [Stop_timer] to stop the loop. *)
let[@inline] run_every_s (self : t) delay f = self.run_every_s delay f

let[@inline] terminate (self : t) : unit = self.terminate ()
