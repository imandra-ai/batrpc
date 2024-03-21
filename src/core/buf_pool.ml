type t = {
  enc: Pbrt.Encoder.t Apool.t;
  bufs: bytes Apool.t;  (** Buffer of at least 16kB *)
  buf_size: int;
}

let default_enc_size_ = 4 * 1024
let default_buf_size_ = 16 * 1024

let create ?(enc_size = default_enc_size_) ?(buf_size = default_buf_size_) () :
    t =
  let enc =
    Apool.create ~clear:Pbrt.Encoder.reset
      ~mk_item:(fun () -> Pbrt.Encoder.create ~size:enc_size ())
      ~max_size:24 ()
  and bufs =
    Apool.create ~mk_item:(fun () -> Bytes.create buf_size) ~max_size:24 ()
  in
  { enc; bufs; buf_size }

let[@inline] with_enc self f = Apool.with_resource self.enc f

let with_buf (self : t) size f =
  if size > self.buf_size then
    (* exceptional size: just allocate it now *)
    f (Bytes.create size)
  else
    Apool.with_resource self.bufs f
