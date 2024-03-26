(** Binary RPC over streams of bytes for protobuf services. *)

(** {2 Core library} *)

include Batrpc_core

(** {2 Unix-based networking implementations} *)

module Batrpc_unix = Batrpc_unix
module Tcp_server = Batrpc_unix.Tcp_server
module Tcp_client = Batrpc_unix.Tcp_client
module Util_pipe = Batrpc_unix.Util_pipe
