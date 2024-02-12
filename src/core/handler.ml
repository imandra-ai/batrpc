open Common_

type header = Meta.header

type ('req, 'res) t = header list -> 'req -> (header list * 'res) Fut.t
(** A basic request handler. *)
