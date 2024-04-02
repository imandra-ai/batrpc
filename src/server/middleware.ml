(** Middlewares.

    A middleware is, conceptually, a transformer from
    a [req -> res Fut.t] (or server handler),
    into another [req -> res Fut.t] with additional features.
*)

type header = Meta.header

type t = {
  handle:
    'req 'req_mode 'res 'res_mode.
    service_name:string ->
    ('req, 'req_mode, 'res, 'res_mode) Service.Server.rpc ->
    ('req, 'res) Handler.t ->
    ('req, 'res) Handler.t;
}
[@@unboxed]
(** Middleware. *)
