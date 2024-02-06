type t = { run_after_s: float -> (unit -> unit) -> unit } [@@unboxed]
(** Timer, to run callbacks in the future *)

let[@inline] run_after_s (self : t) delay f = self.run_after_s delay f
