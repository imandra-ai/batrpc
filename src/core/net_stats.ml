type t = {
  n_sent: Byte_counter.t;
  n_received: Byte_counter.t;
}
[@@deriving show { with_path = false }]
(** Network statistics *)

let create () : t =
  { n_sent = Byte_counter.create (); n_received = Byte_counter.create () }

let n_sent self = self.n_sent

let get_n_sent self : int = Byte_counter.get self.n_sent

let n_received self = self.n_received

let get_n_received self : int = Byte_counter.get self.n_received
