(** Pool of byte resources to reuse *)

type t
(** The pool of encoders and buffers. Thread-safe. *)

val create : ?enc_size:int -> ?buf_size:int -> unit -> t
(** New pool *)

val with_enc : t -> (Pbrt.Encoder.t -> 'a) -> 'a
(** Temporarily acquire an encoder *)

val with_buf : t -> int -> (bytes -> 'a) -> 'a
(** Temporarily acquire a buffer of at least the given size. *)
