let ( let@ ) = ( @@ )
let port = 12345

module RPC = Batrpc
module Fut = Moonpool.Fut

let services =
  [
    Trivial.Swapper.Server.make
      ~swap:(fun rpc ->
        RPC.mk_handler rpc @@ fun (p : Trivial.pair) ->
        let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.swap" in
        Fut.return @@ Trivial.make_pair ~x:p.y ~y:p.x ())
      ~count_chars:(fun rpc ->
        RPC.mk_handler rpc @@ fun (msg : Trivial.big_string) ->
        let n = String.length msg.msg in
        Fut.return @@ Trivial.make_count ~count:(Int32.of_int n) ())
      ();
  ]

let () =
  let active = Switch.create () in
  let timer = Timer.create () in

  (* we need a thread pool to run the tasks *)
  let@ runner = Moonpool.Ws_pool.with_ ~num_threads:8 () in

  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let server : RPC.Tcp_server.t =
    match RPC.Tcp_server.create ~active ~runner ~timer ~services addr with
    | Ok s -> s
    | Error err ->
      let err =
        err |> Error.add_ctx (Error.message "Establishing RPC server")
      in
      Error.raise_err err
  in

  (* background thread to accept connection *)
  Format.eprintf "listening on port %d@." port;
  RPC.Tcp_server.run server
