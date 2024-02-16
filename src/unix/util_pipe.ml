(** Mostly useful for testing: communicate via pipes *)

open Util_

let with_pipe ?close_noerr () (f : Io.In.bufferized_t * Io.Out.t -> 'a) : 'a =
  let fd1, fd2 = Unix.pipe () in
  let ic = new Io.In.bufferized @@ new Io.In.of_fd ?close_noerr fd1 in
  let oc = new Io.Out.bufferized @@ new Io.Out.of_fd ?close_noerr fd2 in

  let@ () =
    Fun.protect ~finally:(fun () ->
        (try Unix.close fd1 with _ -> ());
        try Unix.close fd2 with _ -> ())
  in
  f (ic, oc)
