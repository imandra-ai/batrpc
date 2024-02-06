(** Network metrics *)

module Byte_counter = Batrpc_util.Byte_counter

let n_sent : Byte_counter.t = Byte_counter.create ()
let n_received : Byte_counter.t = Byte_counter.create ()
let[@inline] get_n_sent () : int = Byte_counter.get n_sent
let[@inline] get_n_received () : int = Byte_counter.get n_received
