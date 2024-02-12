module Otel = Opentelemetry

type header = Meta.header

val get_span_ctx : header list -> Otel.Span_ctx.t option
(** Get a span context *)

val header_span_ctx : Otel.Span_ctx.t -> header
(** Make a header for this span context *)

module Server : sig
  val middlewares : unit -> Middleware.Server.t list
end
