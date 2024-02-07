(** Basic timer.

    Runs in a background thread.
*)

include module type of struct
  include Timer
end

val create : unit -> t
