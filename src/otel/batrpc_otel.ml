(* TODO:
   https://www.w3.org/TR/trace-context/
*)

module Otel = Opentelemetry
module Span = Otel.Span

type header = Meta.header

module Util_ = struct
  let enabled = Otel.Collector.has_backend

  let instrument_fut ?kind ?(attrs = []) ~trace_id ~parent_id name
      (mk_fut : unit -> _ Fut.t) : _ Fut.t =
    if enabled () then (
      let trace_id =
        match trace_id with
        | None -> Otel.Trace_id.create ()
        | Some tid -> tid
      in
      let now = Otel.Timestamp_ns.now_unix_ns () in
      let span_id = Otel.Span_id.create () in

      let on_fut_result_ res =
        let status =
          match res with
          | Ok _ -> Otel.Proto.Trace.default_status ~code:Status_code_ok ()
          | Error (exn, _bt) ->
            Otel.Proto.Trace.default_status ~code:Status_code_error
              ~message:(Printexc.to_string exn) ()
        in
        let span, _ =
          Otel.Span.create ?kind ~trace_id ?parent:parent_id ~id:span_id ~attrs
            ~events:[] ~start_time:now
            ~end_time:(Otel.Timestamp_ns.now_unix_ns ())
            ~status name
        in
        Otel.Trace.emit [ span ]
      in

      let fut = mk_fut () in
      Fut.on_result fut on_fut_result_;
      fut
    ) else
      mk_fut ()
end

let get_span_ctx headers =
  match List.find_opt (fun h -> h.Meta.key = "traceparent") headers with
  | Some h ->
    let ctx =
      Otel.Span_ctx.of_w3c_trace_context_exn (Bytes.unsafe_of_string h.value)
    in
    Some ctx
  | None -> None

let header_span_ctx (sp : Otel.Span_ctx.t) : header =
  Meta.make_header ~key:"traceparent"
    ~value:(Bytes.unsafe_to_string @@ Otel.Span_ctx.to_w3c_trace_context sp)
    ()

module Server = struct
  (** Middleware that instruments request handlers *)
  let trace_ : Middleware.Server.t =
    {
      handle =
        (fun ~service_name rpc handler headers req : _ Fut.t ->
          let trace_id, parent_id =
            match get_span_ctx headers with
            | None -> None, None
            | Some sp ->
              ( Some (Otel.Span_ctx.trace_id sp),
                Some (Otel.Span_ctx.parent_id sp) )
          in
          Util_.instrument_fut
            ~attrs:[ "service", `String service_name ]
            ~kind:Span.Span_kind_server ~trace_id ~parent_id ("rpc." ^ rpc.name)
            (fun () -> handler headers req));
    }

  let middlewares () : _ list = [ trace_ ]
end