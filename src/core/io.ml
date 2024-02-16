(** IO primitives *)

let default_buf_size = 4 * 1024

module In = struct
  (** Input stream *)
  class type t =
    object
      method input : bytes -> int -> int -> int
      method really_input : bytes -> int -> int -> unit
      method close : unit -> unit
    end

  open struct
    class virtual base =
      object (self)
        method virtual input : bytes -> int -> int -> int

        (* provide this loop *)
        method really_input bs i len0 =
          let i = ref i in
          let len = ref len0 in
          while !len > 0 do
            let n = self#input bs !i !len in
            if n = 0 then raise End_of_file;
            i := !i + n;
            len := !len - n
          done
      end
  end

  class of_str (str : string) : t =
    let off = ref 0 in
    object
      method close () = ()

      method input bs i len =
        let n = min len (String.length str - !off) in
        Bytes.blit_string str !off bs i n;
        off := !off + n;
        n

      inherit base
    end

  class of_fd ?(shutdown = false) ?n_received ?(close_noerr = false)
    (fd : Unix.file_descr) : t =
    object
      method input bs i len =
        let n = Unix.read fd bs i len in
        Byte_counter.add_opt n_received n;
        n

      inherit base

      method close () =
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_RECEIVE with _ -> ()
        );
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end

  class type bufferized_t =
    object
      inherit t
      method read_line : unit -> string option
    end

  class bufferized ?(buf = Bytes.create default_buf_size) (ic : #t) :
    bufferized_t =
    let buf_off = ref 0 in
    let buf_len = ref 0 in
    let eof = ref false in
    let buffer = Buffer.create 32 in

    let refill_ () =
      if not !eof then (
        buf_off := 0;
        buf_len := ic#input buf 0 (Bytes.length buf);
        if !buf_len = 0 then eof := true
      )
    in
    object
      method input bs i len =
        if !buf_len = 0 then refill_ ();
        let n = min len !buf_len in
        Bytes.blit buf !buf_off bs i n;
        buf_off := !buf_off + n;
        buf_len := !buf_len - n;
        n

      inherit base
      method close () = ic#close ()

      method read_line () =
        Buffer.clear buffer;
        let continue = ref true in
        while !continue && not !eof do
          match
            String.index_from_opt (Bytes.unsafe_to_string buf) !buf_off '\n'
          with
          | Some i when i - !buf_off < !buf_len ->
            let n = i - !buf_off in
            Buffer.add_subbytes buffer buf !buf_off n;
            buf_off := !buf_off + n + 1;
            buf_len := !buf_len - n - 1;
            continue := false
          | _ ->
            Buffer.add_subbytes buffer buf !buf_off !buf_len;
            buf_off := 0;
            buf_len := 0;
            refill_ ()
        done;
        if !eof && Buffer.length buffer = 0 then
          None
        else
          Some (Buffer.contents buffer)
    end

  let read_lines (self : #bufferized_t) : string list =
    let rec loop acc =
      match self#read_line () with
      | None -> List.rev acc
      | Some line -> loop (line :: acc)
    in
    loop []
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

  class bufferized ?(buf = Bytes.create default_buf_size) (oc : #t) : t =
    let off = ref 0 in
    let flush_ () =
      if !off > 0 then (
        oc#output buf 0 !off;
        off := 0
      )
    in
    let[@inline] maybe_flush_ () = if !off = Bytes.length buf then flush_ () in

    object
      method flush () = flush_ ()

      method output bs i len : unit =
        let i = ref i in
        let len = ref len in
        while !len > 0 do
          maybe_flush_ ();
          let n = min !len (Bytes.length buf - !off) in
          assert (n > 0);

          Bytes.blit bs !i buf !off !len;
          i := !i + n;
          len := !len - n;
          off := !off + n
        done;
        maybe_flush_ ()

      method close () =
        flush_ ();
        oc#close ()
    end

  class of_buffer (buf : Buffer.t) : t =
    object
      method output bs i len = Buffer.add_subbytes buf bs i len
      method flush () = ()
      method close () = ()
    end
end
