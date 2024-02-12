open Common_

type ('req, 'res) t = 'req -> 'res Fut.t
(** A basic request handler. *)
