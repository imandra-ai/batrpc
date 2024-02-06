(** Middlewares.

    A middleware is, conceptually, a transformer from
    a [req -> res Fut.t] (client method, or server handler),
    into another [req -> res Fut.t] with additional features.
*)

open Common_

type t = {
  handle:
    'req 'req_mode 'res 'res_mode.
    service_name:string ->
    ('req, 'req_mode, 'res, 'res_mode) Handler.t ->
    ('req, 'req_mode, 'res, 'res_mode) Handler.t;
}
[@@unboxed]
(** Middleware. *)

(** Basic local tracing. *)
let tracing : t =
  {
    handle =
      (fun ~service_name (h : _ Handler.t) rpc req ->
        let _sp =
          Trace.enter_span ~__FILE__ ~__LINE__ "rpc-handler" ~data:(fun () ->
              [ "service", `String service_name; "meth", `String rpc.rpc_name ])
        in

        let fut = h rpc req in
        Fut.on_result fut (fun _ -> Trace.exit_span _sp);

        fut);
  }
