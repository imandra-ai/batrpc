(** Basic server for a single client, on an existing connection. *)

open Common_

type t = Rpc_conn.t
type handler = Rpc_conn.handler

let create :
    ?active:Switch.t ->
    ?middlewares:Middleware.Server.t list ->
    encoding:Encoding.t ->
    services:handler Service.Server.t list ->
    runner:Runner.t ->
    timer:Timer.t ->
    ic:#Io.In.t ->
    oc:#Io.Out.t ->
    unit ->
    t =
 fun ?active ?(middlewares = []) ~encoding ~services ~runner ~timer ~ic ~oc () ->
  let rpc = Rpc_conn.create ?active ~encoding ~runner ~timer ~ic ~oc () in
  let server = Rpc_conn.server_state rpc in
  List.iter (Server_state.add_middleware server) middlewares;
  List.iter (Server_state.add_service server) services;
  rpc

let close : t -> unit = Rpc_conn.close_and_join
