(** Basic server for a single client, on an existing connection. *)

open Common_

type t = Rpc_conn.t

let create :
    ?active:Switch.t ->
    ?middlewares:Middleware.Sync.t list ->
    services:Service.Server.t list ->
    executor:Executor.t ->
    timer:Timer.t ->
    ic:Io.In.t ->
    oc:Io.Out.t ->
    unit ->
    t =
 fun ?active ?middlewares ~services ~executor ~timer ~ic ~oc () ->
  let server_state = Server_state.create ?middlewares ~services () in
  Rpc_conn.create ?active ~server_state ~executor ~timer ~ic ~oc ()

let close : t -> unit = Rpc_conn.close_and_join
