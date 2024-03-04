(** Basic client that doesn't have a server component *)

type t = Rpc_conn.t

let create :
    ?buf_pool:Buf_pool.t ->
    ?active:Switch.t ->
    ?encoding:Encoding.t ->
    runner:Moonpool.Runner.t ->
    timer:Timer.t ->
    ic:#Io.In.t ->
    oc:#Io.Out.t ->
    unit ->
    t =
 fun ?buf_pool ?active ?(encoding = Encoding.Binary) ~runner ~timer ~ic ~oc () :
     t ->
  Encoding.write_to_oc oc encoding;
  Rpc_conn.create ?buf_pool ~encoding ~runner ?active ~timer ~ic ~oc ()

let close_and_join = Rpc_conn.close_and_join
let close_without_joining = Rpc_conn.close_without_joining
let call = Rpc_conn.call
let call_client_stream = Rpc_conn.call_client_stream
let call_server_stream = Rpc_conn.call_server_stream
