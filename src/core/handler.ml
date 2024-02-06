open Common_

type ('req, 'req_mode, 'res, 'res_mode) t =
  ('req, 'req_mode, 'res, 'res_mode) Service.Client.rpc -> 'req -> 'res Fut.t
(** A handler. *)
