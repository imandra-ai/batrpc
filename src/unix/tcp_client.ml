type t = Rpc_conn.t

let connect ?active ?buf_pool ?middlewares
    ?(executor = Executor.immediate_local ()) ?(services = []) ?net_stats ~timer
    (addr : Unix.sockaddr) : t Error.result =
  let@ () =
    Error.guardf (fun k ->
        k "Connecting to RPC server on %s" (Util_.string_of_sockaddr addr))
  in
  let@ () = Error.try_catch ~kind:NetworkError () in

  let kind = Util_.kind_of_sockaddr addr in
  let sock = Unix.socket kind Unix.SOCK_STREAM 0 in

  Unix.setsockopt sock Unix.TCP_NODELAY true;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  Unix.connect sock addr;

  let ic =
    new Io.In.of_in_channel
      ~close_noerr:true
      ?n_received:(Option.map Net_stats.n_received net_stats)
    @@ Unix.in_channel_of_descr sock
  in
  let oc =
    new Io.Out.of_out_channel
      ~close_noerr:true
      ?n_sent:(Option.map Net_stats.n_sent net_stats)
    @@ Unix.out_channel_of_descr sock
  in

  let client_state = Client_state.create ?middlewares () in
  let conn : t =
    Rpc_conn.create ?active ?buf_pool ~client_state ~executor ~timer ~ic ~oc ()
  in
  List.iter (Rpc_conn.add_service conn) services;

  conn

let close_and_join = Rpc_conn.close_and_join

let close_without_joining = Rpc_conn.close_without_joining
