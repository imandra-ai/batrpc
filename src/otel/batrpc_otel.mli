module Otel = Opentelemetry

type header = Meta.header

val get_span_ctx : header list -> Otel.Span_ctx.t option
(** Get a span context *)

val header_span_ctx : Otel.Span_ctx.t -> header
(** Make a header for this span context *)

val k_span_ctx : Otel.Span_ctx.t Hmap.key
(** Key for trace context. The middleware will
    set this key if an incoming span context was parsed. *)

module Server : sig
  val middlewares : unit -> Server.Middleware.t list
end
