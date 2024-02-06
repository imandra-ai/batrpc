(** Mostly useful for testing: communicate via pipes *)

open Common_

let with_pipe ?close_noerr () (f : Io.In.t * Io.Out.t -> 'a) : 'a =
  let fd1, fd2 = Unix.pipe () in
  let ic =
    new Io.In.of_in_channel ?close_noerr @@ Unix.in_channel_of_descr fd1
  in
  let oc =
    new Io.Out.of_out_channel ?close_noerr @@ Unix.out_channel_of_descr fd2
  in

  let@ () =
    Fun.protect ~finally:(fun () ->
        (try Unix.close fd1 with _ -> ());
        try Unix.close fd2 with _ -> ())
  in
  f (ic, oc)
