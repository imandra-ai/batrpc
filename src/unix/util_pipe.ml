(** Mostly useful for testing: communicate via pipes *)

let with_pipe ?close_noerr () (f : Io.In.t * Io.Out.t -> 'a) : 'a =
  let fd1, fd2 = Unix.pipe () in
  let ic = new Io.In.of_fd ?close_noerr fd1 in
  let oc = new Io.Out.of_fd ?close_noerr fd2 in

  let@ () =
    Fun.protect ~finally:(fun () ->
        (try Unix.close fd1 with _ -> ());
        try Unix.close fd2 with _ -> ())
  in
  f (ic, oc)
