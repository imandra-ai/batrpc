(** Middlewares.

    A middleware is, conceptually, a transformer from
    a [req -> res Fut.t] (client method),
    into another [req -> res Fut.t] with additional features.
*)

type t = {
  handle:
    'req 'req_mode 'res 'res_mode.
    ('req, 'req_mode, 'res, 'res_mode) Service.Client.rpc ->
    ('req, 'res) Handler.t ->
    ('req, 'res) Handler.t;
}
[@@unboxed]
(** Middleware. *)

(** Basic local tracing. *)
let tracing : t =
  {
    handle =
      (fun rpc (h : _ Handler.t) req ->
        let _sp =
          Trace.enter_span ~__FILE__ ~__LINE__ "rpc-handler" ~data:(fun () ->
              [
                "service", `String rpc.service_name;
                "meth", `String rpc.rpc_name;
              ])
        in

        let fut = h req in
        Fut.on_result fut (fun _ -> Trace.exit_span _sp);

        fut);
  }
