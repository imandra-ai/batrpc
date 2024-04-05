(** Network metrics *)

module M = Imandrakit_metrics

type t = int M.Counter.t

let m_received : t = M.Counter.create_int "rpc.received.B"
let m_sent : t = M.Counter.create_int "rpc.sent.B"
let add : t -> int -> unit = M.Counter.incr_by

let[@inline] add_opt (self : t option) (n : int) : unit =
  match self with
  | None -> ()
  | Some c -> M.Counter.incr_by c n
