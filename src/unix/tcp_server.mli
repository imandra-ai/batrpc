(** TCP server.

    The server will spawn a thread per client connection, and use
    an executor to handle individual requests from each client. *)

open Util_

type t

val create :
  ?server_state:Server_state.t ->
  ?on_new_client:(Rpc_conn.t -> Unix.sockaddr -> unit) ->
  ?config_socket:(Unix.file_descr -> unit) ->
  ?reuseaddr:bool ->
  ?net_stats:Net_stats.t ->
  active:Switch.t ->
  executor:Executor.t ->
  timer:Timer.t ->
  services:Service.Server.t list ->
  Unix.sockaddr ->
  t Error.result
(** Create and bind the TCP server.
    @param executor used to handle individual requests.
    @param config_socket custom function to set options on server socket
      before it's used, but after basic options and [reuseaddr] have been set. *)

val active : t -> Switch.t

val run : t -> unit
(** Run the server in current thread. This blocks the current thread
   for potentially a very long time. *)

val terminate : t -> unit
(** Terminate server. *)
