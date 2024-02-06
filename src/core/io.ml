(** IO primitives *)

module In = struct
  (** Input stream *)
  class type t =
    object
      method input : bytes -> int -> int -> int

      method really_input : bytes -> int -> int -> unit

      method close : unit -> unit
    end

  class of_in_channel ?n_received ?(close_noerr = false) (ic : in_channel) : t =
    object
      method input bs i len =
        let n = input ic bs i len in
        Byte_counter.add_opt n_received n;
        n

      method really_input bs i len =
        really_input ic bs i len;
        Byte_counter.add_opt n_received len

      method close () =
        if close_noerr then
          close_in_noerr ic
        else
          close_in ic
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

  class of_out_channel ?n_sent ?(close_noerr = false) (oc : out_channel) : t =
    object
      method output bs i len =
        output oc bs i len;
        Byte_counter.add_opt n_sent len

      method flush () = flush oc

      method close () =
        if close_noerr then
          close_out_noerr oc
        else
          close_out oc
    end

  class of_buffer (buf : Buffer.t) : t =
    object
      method output bs i len = Buffer.add_subbytes buf bs i len

      method flush () = ()

      method close () = ()
    end
end
