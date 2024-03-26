type t = Rpc_conn.t

let connect ?active ?buf_pool ?(middlewares = []) ?(services = [])
    ?(encoding = Encoding.Binary) ~runner ~timer (addr : Unix.sockaddr) :
    t Error.result =
  let@ () =
    Error.guardf (fun k ->
        k "Connecting to RPC server on %s" (Util_sockaddr.show addr))
  in
  let@ () = Error.try_catch ~kind:Errors.network () in

  let kind = Util_sockaddr.kind addr in
  let sock = Unix.socket kind Unix.SOCK_STREAM 0 in

  Unix.setsockopt sock Unix.TCP_NODELAY true;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  Unix.connect sock addr;

  let ic =
    new Io.In.of_fd
      ~shutdown:true ~close_noerr:true ~n_received:Net_stats.n_received sock
  in
  let oc = new Io.Out.of_fd ~close_noerr:true ~n_sent:Net_stats.n_sent sock in
  (* wire format negociation *)
  Encoding.write_to_oc oc encoding;

  let conn : t =
    Rpc_conn.create ?active ?buf_pool ~encoding ~runner ~timer ~ic ~oc ()
  in
  let client = Rpc_conn.client_state conn in
  List.iter (Client_state.add_middleware client) middlewares;
  List.iter (Rpc_conn.add_service conn) services;

  conn

let close_and_join = Rpc_conn.close_and_join
let close_without_joining = Rpc_conn.close_without_joining

let with_connect ?active ?buf_pool ?middlewares ?services ?encoding ~runner
    ~timer addr f =
  match
    connect ?active ?buf_pool ?middlewares ?services ?encoding ~runner ~timer
      addr
  with
  | Ok c ->
    let finally () = close_and_join c in
    Fun.protect ~finally (fun () -> f c)
  | Error err -> Error.raise_err err
