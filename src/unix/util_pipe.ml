(** Mostly useful for testing: communicate via pipes *)

open Common_

let with_pipe ?close_noerr () (f : Io.In.t * Io.Out.t -> 'a) : 'a =
  let fd1, fd2 = Unix.pipe () in
  let fd1 = Fd.create fd1 in
  let fd2 = Fd.create fd2 in
  MIO.Unix.set_nonblock fd1;
  MIO.Unix.set_nonblock fd2;
  let ic = new Io.In.of_fd ?close_noerr fd1 in
  let oc = new Io.Out.of_fd ?close_noerr fd2 in

  let@ () =
    Fun.protect ~finally:(fun () ->
        (try MIO.Unix.close fd1 with _ -> ());
        try MIO.Unix.close fd2 with _ -> ())
  in
  f (ic, oc)
