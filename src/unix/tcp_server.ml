open Util_

type t = {
  active: Switch.t;
  alive: bool Atomic.t;
  st: Server_state.t;
  on_new_client: Rpc_conn.t -> Unix.sockaddr -> unit;
  sock: Unix.file_descr;
  runner: Runner.t;
  timer: Timer.t;
  alive_conns: int Atomic.t;
  buf_pool: Buf_pool.t;
}

let[@inline] active self = self.active

let terminate self : unit =
  if Atomic.exchange self.alive false then (
    Log_rpc.debug (fun k -> k "tcp server: terminating");
    try Unix.close self.sock with _ -> ()
  )

let add_middleware self m = Server_state.add_middleware self.st m

let create ?server_state ?(on_new_client = fun _ _ -> ())
    ?(config_socket = ignore) ?(reuseaddr = true) ?(middlewares = []) ~active
    ~runner ~timer ~services (addr : Unix.sockaddr) : t Error.result =
  let@ () = Error.try_catch ~kind:Errors.network () in
  let kind = Util_sockaddr.kind addr in
  let sock = Unix.socket kind Unix.SOCK_STREAM 0 in

  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;
  if reuseaddr then Unix.setsockopt sock Unix.SO_REUSEPORT true;

  config_socket sock;

  (* we're going to use [select] to not entirely block on [accept] *)
  Unix.set_nonblock sock;

  Unix.bind sock addr;
  Unix.listen sock 16;

  let buf_pool = Buf_pool.create () in

  let st =
    match server_state with
    | Some s ->
      List.iter (Server_state.add_service s) services;
      s
    | None -> Server_state.create ~services ()
  in

  let self =
    {
      active;
      buf_pool;
      alive = Atomic.make true;
      on_new_client;
      runner;
      st;
      timer;
      sock;
      alive_conns = Atomic.make 0;
    }
  in
  List.iter (add_middleware self) middlewares;

  Switch.on_turn_off active (fun () -> terminate self);
  self

(** Handle this new client. This only sets up the new connection,
    actual work will happen in the new RPC conn. *)
let handle_client_async_ (self : t) client_sock client_addr : unit =
  Unix.setsockopt client_sock Unix.TCP_NODELAY true;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  let ic =
    new Io.In.of_fd
      ~shutdown:true ~close_noerr:true ~n_received:Net_stats.n_received
      client_sock
  in
  let oc =
    new Io.Out.of_fd
      ~shutdown:true ~close_noerr:true ~n_sent:Net_stats.n_sent client_sock
  in

  let magic_number =
    let bs4 = Bytes.create 4 in
    Io.In.really_input ic bs4 0 4;
    Bytes.get_int32_le bs4 0
  in

  let encoding =
    match Encoding.of_int32_le magic_number with
    | Some e -> e
    | None ->
      Error.failf ~kind:Errors.protocol "Rpc_conn: invalid magic number %ld"
        magic_number
  in

  (* spawn a background thread, using the same [active] so as
     to propagate cancellation to it *)
  let rpc_conn : Rpc_conn.t =
    Rpc_conn.create ~active:self.active ~buf_pool:self.buf_pool
      ~server_state:self.st ~runner:self.runner ~encoding ~timer:self.timer ~ic
      ~oc ()
  in

  Fut.on_result (Rpc_conn.on_close rpc_conn) (fun _ ->
      Atomic.decr self.alive_conns);

  (* optional callback *)
  self.on_new_client rpc_conn client_addr;

  ()

let run (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "bin-rpc.tcp-server.run" in

  let wait_for_client_or_timeout () : bool =
    match Unix.select [ self.sock ] [] [] 1.0 with
    | [], _, _ -> false
    | _ -> true
  in

  while Atomic.get self.alive && Switch.is_on self.active do
    let maybe_client = wait_for_client_or_timeout () in
    if maybe_client then (
      match Unix.accept self.sock with
      | client_sock, client_addr ->
        Atomic.incr self.alive_conns;

        Log_rpc.debug (fun k ->
            k "(@[tcp-server.run.accept-client@ :on %s@])"
              (Util_sockaddr.show client_addr));

        handle_client_async_ self client_sock client_addr
      | exception Sys_error msg ->
        Log_rpc.warn (fun k ->
            k "Tcp_server: could not accept new connection: %s" msg);
        terminate self
      | exception exn ->
        Log_rpc.err (fun k ->
            k "Tcp_server: error when accepting connection: %s"
              (Printexc.to_string exn));
        terminate self
    )
  done
