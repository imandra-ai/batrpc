module C = Batrpc_client

type t = C.t

let connect ?active ?buf_pool ?(middlewares = []) ?(encoding = Encoding.Binary)
    ~timer (addr : Unix.sockaddr) : t Error.result =
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

  let ic = new Io.In.of_fd ~shutdown:true ~close_noerr:true sock in
  let oc = new Io.Out.of_fd ~close_noerr:true sock in
  (* wire format negociation *)
  Encoding.write_to_oc oc encoding;

  let conn : t = C.create ?active ?buf_pool ~encoding ~timer ~ic ~oc () in
  let client = C.state conn in
  List.iter (C.State.add_middleware client) middlewares;

  conn

let close_and_join = C.close_and_join
let close_without_joining = C.close_without_joining

let with_connect ?active ?buf_pool ?middlewares ?encoding ~timer addr
    (f : t -> 'a) : 'a =
  match connect ?active ?buf_pool ?middlewares ?encoding ~timer addr with
  | Ok c ->
    let finally () = close_and_join c in
    Fun.protect ~finally (fun () -> f c)
  | Error err -> Error.raise_err err
