open Common_

type t = { n_bytes: int Atomic.t } [@@unboxed]
(** Byte counter. *)

let pp out (self : t) =
  Format.fprintf out "{n_bytes=%d}" (Atomic.get self.n_bytes)

let create () : t = { n_bytes = Atomic.make 0 }

let[@inline] add (self : t) n : unit =
  ignore (Atomic.fetch_and_add self.n_bytes n : int)

let[@inline] add_opt (self : t option) n : unit =
  match self with
  | None -> ()
  | Some ns -> add ns n

let[@inline] get self = Atomic.get self.n_bytes
