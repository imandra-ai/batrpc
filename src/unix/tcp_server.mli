(** TCP server.

    The server will spawn a thread per client connection, and use
    an executor to handle individual requests from each client. *)

module Server = Batrpc_server

type conn = Server.For_client.t
(** A single connection to a client *)

type t
(** The server itself *)

val create :
  ?server_state:Server.State.t ->
  ?on_new_client:(conn -> Unix.sockaddr -> unit) ->
  ?config_socket:(Unix.file_descr -> unit) ->
  ?reuseaddr:bool ->
  ?middlewares:Server.Middleware.t list ->
  ?config:Server.Config.t ->
  active:Switch.t ->
  runner:Runner.t ->
  timer:Timer.t ->
  services:Server.State.handler Service.Server.t list ->
  Unix.sockaddr ->
  t Error.result
(** Create and bind the TCP server.
    @param executor used to handle individual requests.
    @param config_socket custom function to set options on server socket
      before it's used, but after basic options and [reuseaddr] have been set. *)

val add_middleware : t -> Server.Middleware.t -> unit
val active : t -> Switch.t

val run : t -> unit
(** Run the server in current thread. This blocks the current thread
   for potentially a very long time. *)

val terminate : t -> unit
(** Terminate server. *)
