(** Binary RPC over streams of bytes for protobuf services. *)

include Batrpc_core
module Tcp_server = Batrpc_unix.Tcp_server
module Tcp_client = Batrpc_unix.Tcp_client
