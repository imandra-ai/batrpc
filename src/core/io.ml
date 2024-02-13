(** IO primitives *)

module In = struct
  (** Input stream *)
  class type t =
    object
      method input : bytes -> int -> int -> int
      method really_input : bytes -> int -> int -> unit
      method close : unit -> unit
    end

  class of_fd ?(shutdown = false) ?n_received ?(close_noerr = false)
    (fd : Unix.file_descr) : t =
    object
      method input bs i len =
        let n = Unix.read fd bs i len in
        Byte_counter.add_opt n_received n;
        n

      method really_input bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          let n = Unix.read fd bs !i !len in
          i := !i + n;
          len := !len - n
        done;
        Byte_counter.add_opt n_received len0

      method close () =
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_RECEIVE with _ -> ()
        );
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end
end

module Out = struct
  (** Output stream *)
  class type t =
    object
      method output : bytes -> int -> int -> unit
      method flush : unit -> unit
      method close : unit -> unit
    end

  class of_fd ?(shutdown = false) ?n_sent ?(close_noerr = false)
    (fd : Unix.file_descr) : t =
    object
      method output bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          let n = Unix.write fd bs !i !len in
          i := !i + n;
          len := !len - n
        done;
        Byte_counter.add_opt n_sent len0

      method flush () = ()

      method close () =
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_SEND with _ -> ()
        );

        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end

  class of_buffer (buf : Buffer.t) : t =
    object
      method output bs i len = Buffer.add_subbytes buf bs i len
      method flush () = ()
      method close () = ()
    end
end
