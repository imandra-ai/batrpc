(** IO primitives *)

module Slice = Iostream.Slice

(** Input stream *)
module In = struct
  include Iostream.In_buf

  class of_str (str : string) : t = of_string str

  class of_fd ?(shutdown = false) ?(close_noerr = false) ?bytes
    (fd : Unix.file_descr) : t =
    let eof = ref false in
    object
      inherit t_from_refill ?bytes ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.len <- Unix.read fd slice.bytes 0 (Bytes.length slice.bytes);
          slice.off <- 0;
          if slice.len = 0 then eof := true
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

  (** [instrument ic ~on_read] makes a new buffered input stream.
      @param on_read is called at every read from [ic]. *)
  class instrument ?bytes (ic : #Iostream.In.t)
    ~(on_read : bytes -> int -> int -> unit) : t =
    let eof = ref false in
    object
      inherit t_from_refill ?bytes ()

      method close () =
        eof := true;
        Iostream.In.close ic

      method private refill (slice : Slice.t) =
        if not !eof then (
          let n =
            Iostream.In.input ic slice.bytes 0 (Bytes.length slice.bytes)
          in
          slice.len <- n;
          on_read slice.bytes 0 n;

          if slice.len = 0 then eof := true
        )
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

  class of_fd ?(shutdown = false) ?(close_noerr = false) (fd : Unix.file_descr) :
    t =
    object
      inherit t_from_output ()

      method private output_underlying bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          let n = Unix.write fd bs !i !len in
          i := !i + n;
          len := !len - n
        done

      method private close_underlying () =
        if shutdown then (
          try Unix.shutdown fd Unix.SHUTDOWN_SEND with _ -> ()
        );

        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end

  (** [instrument oc ~on_write] returns a new output stream
      that forwards to [oc].
      @param on_write is called at every write. *)
  class instrument (oc : #Iostream.Out.t)
    ~(on_write : bytes -> int -> int -> unit) : t =
    object
      inherit t_from_output ()

      method private output_underlying bs i len =
        on_write bs i len;
        Iostream.Out.output oc bs i len

      method private close_underlying () = Iostream.Out.close oc
    end

  let output_line (self : #t) str : unit =
    output_string self str;
    self#output_char '\n'
end
