(** IO primitives *)

module Slice = Iostream.Slice

(** Input stream *)
module In = struct
  include Iostream.In_buf

  class of_str (str : string) : t = of_string str

  class of_fd ?(shutdown = false) ?n_received ?(close_noerr = false)
    (fd : Unix.file_descr) : t =
    let eof = ref false in
    object
      inherit t_from_refill ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.len <- Unix.read fd slice.bytes 0 (Bytes.length slice.bytes);
          slice.off <- 0;
          if slice.len = 0 then eof := true;
          Byte_counter.add_opt n_received slice.len
        )

      method close () =
        eof := true;
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_RECEIVE with _ -> ()
        );
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end

  let read_lines (self : #t) : string list =
    let rec loop acc =
      match input_line self with
      | None -> List.rev acc
      | Some line -> loop (line :: acc)
    in
    loop []

  (** Input this exact number of bytes.
      @raise End_of_file if EOF was reached before reading all the bytes. *)
  let really_input (self : #t) bs i len0 =
    let i = ref i in
    let len = ref len0 in
    while !len > 0 do
      let n = self#input bs !i !len in
      if n = 0 then raise End_of_file;
      i := !i + n;
      len := !len - n
    done
end

module Out = struct
  include Iostream.Out_buf

  class of_fd ?(shutdown = false) ?n_sent ?(close_noerr = false)
    (fd : Unix.file_descr) : t =
    object
      inherit t_from_output ()

      method private output_underlying bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          let n = Unix.write fd bs !i !len in
          i := !i + n;
          len := !len - n
        done;
        Byte_counter.add_opt n_sent len0

      method private close_underlying () =
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_SEND with _ -> ()
        );

        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end

  let output_line (self : #t) str : unit =
    output_string self str;
    self#output_char '\n'
end
