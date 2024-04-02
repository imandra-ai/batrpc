(** Binary RPC over streams of bytes for protobuf services. *)

(** {2 Core library} *)

include Batrpc_core
module Client = Batrpc_client
module Server = Batrpc_server

let mk_handler = Server.mk_handler
let mk_handler_full = Server.mk_handler_full
let mk_server_stream_handler = Server.mk_server_stream_handler
let mk_client_stream_handler = Server.mk_client_stream_handler

(** {2 Unix-based networking implementations} *)

module Batrpc_unix = Batrpc_unix
module Tcp_server = Batrpc_unix.Tcp_server
module Tcp_client = Batrpc_unix.Tcp_client
module Util_pipe = Batrpc_unix.Util_pipe
