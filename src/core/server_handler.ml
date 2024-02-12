(** Server-side request handler *)

open Common_

type ('req, 'res, 'state) client_stream_handler_with_state = {
  init: unit -> 'state;
  on_item: 'state -> 'req -> unit;
  on_close: 'state -> 'res;
}

type ('req, 'res) client_stream_handler =
  | Client_stream_handler :
      ('req, 'res, 'state) client_stream_handler_with_state
      -> ('req, 'res) client_stream_handler
[@@unboxed]

type ('req, 'res) server_stream_handler = 'req -> 'res Push_stream.t -> unit

type ('req, 'res, 'state) bidirectional_stream_handler_with_state = {
  init: unit -> 'res Push_stream.t -> 'state;
  on_item: 'state -> 'req -> unit;
  on_close: 'state -> unit;
}

type ('req, 'res) bidirectional_stream_handler =
  | Bidirectional_stream_handler :
      ('req, 'res, 'state) bidirectional_stream_handler_with_state
      -> ('req, 'res) bidirectional_stream_handler
[@@unboxed]

type ('req, 'res) handler_with_ty =
  | Unary of ('req, 'res) Handler.t
  | Client_stream of ('req, 'res) client_stream_handler
  | Server_stream of ('req, 'res) server_stream_handler
  | Bidirectional_stream of ('req, 'res) bidirectional_stream_handler

(** A type-erased request handler *)
type t =
  | Handler : {
      rpc: ('req, _, 'res, _) Service.Server.rpc;
      h: ('req, 'res) handler_with_ty;
    }
      -> t
