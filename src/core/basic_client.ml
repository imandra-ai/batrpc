(** Basic client that doesn't have a server component *)

type t = Rpc_conn.t

let create :
    ?client_state:Client_state.t ->
    ?buf_pool:Buf_pool.t ->
    ?active:Switch.t ->
    timer:Timer.t ->
    ic:Io.In.t ->
    oc:Io.Out.t ->
    unit ->
    t =
 fun ?client_state ?buf_pool ?active ~timer ~ic ~oc () : t ->
  (* no need for a fancy executor, we won't be serving stuff to the other side *)
  let executor = Executor.immediate_local () in
  Rpc_conn.create ?server_state:None ?client_state ?buf_pool ~executor ?active
    ~timer ~ic ~oc ()

let close_and_join = Rpc_conn.close_and_join

let close_without_joining = Rpc_conn.close_without_joining

let call = Rpc_conn.call

let call_client_stream = Rpc_conn.call_client_stream

let call_server_stream = Rpc_conn.call_server_stream
