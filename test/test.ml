module RPC = Batrpc
module Client = RPC.Basic_client
module Fut = Moonpool.Fut
module Fmt = CCFormat

let ( let@ ) = ( @@ )

let () =
  Printexc.register_printer (function
    | RPC.Error.E e -> Some (RPC.Error.show e)
    | _ -> None)

let timer = RPC.Simple_timer.create ()

let services =
  [
    Trivial.Swapper.Server.make
      ~swap:(fun rpc ->
        RPC.mk_handler rpc @@ fun (p : Trivial.pair) ->
        let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.swap" in
        Option.iter Thread.delay p.artificial_delay_s;
        (*
          Thread.yield ();
          Thread.delay (Option.value ~default:0. p.artificial_delay_s);
          *)
        Fut.return @@ Trivial.make_pair ~x:p.y ~y:p.x ())
      ~count_chars:(fun rpc ->
        RPC.mk_handler rpc @@ fun (msg : Trivial.big_string) ->
        let n = String.length msg.msg in
        Fut.return @@ Trivial.make_count ~count:(Int32.of_int n) ())
      ();
    Trivial.Beancount.Server.make
      ~add_stream:(fun rpc ->
        RPC.mk_client_stream_handler rpc
          ~init:(fun () -> ref 0)
          ~on_item:(fun st (x : Trivial.single_int) ->
            Trace.messagef (fun k -> k "server got stream item %ld" x.i);
            st := !st + Int32.to_int x.i)
          ~on_close:(fun st ->
            Trace.message "server got close";
            Trivial.make_count ~count:(Int32.of_int !st) ())
          ())
      ~get_smaller_ints:(fun rpc ->
        RPC.mk_server_stream_handler rpc
        @@ fun (i : Trivial.count) (push : _ RPC.Push_stream.t) : unit ->
        let@ _sp =
          Trace_core.with_span ~__FILE__ ~__LINE__
            "server.get-smaller-ints.handle"
        in
        for i = Int32.to_int i.count downto 0 do
          let@ _sp =
            Trace_core.with_span ~__FILE__ ~__LINE__
              "server.get-smaller-ints.send" ~data:(fun () -> [ "i", `Int i ])
          in
          RPC.Push_stream.push push
            (Trivial.make_single_int ~i:(Int32.of_int i) ());
          Thread.delay 0.000_5
        done;
        Trace.message "server.close";
        RPC.Push_stream.close push)
      ();
  ]

let run_tests_on ~client () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run-tests" in
  Fmt.printf "start tests…@.";

  let p0 = Trivial.make_pair ~x:"hello" ~y:"world" () in
  let fut_p0_swapped =
    Client.call client ~timeout_s:2. Trivial.Swapper.Client.swap p0
  in
  let p0_swapped = Fut.wait_block_exn fut_p0_swapped in
  Fmt.printf "@[@[<h>swap %a@] =@ @[<h>%a@]@]@." Trivial.pp_pair p0
    Trivial.pp_pair p0_swapped;
  assert (p0_swapped.x = "world");
  assert (p0_swapped.y = "hello");

  (* test timeout *)
  let p1 =
    Trivial.make_pair ~x:"hello" ~y:"world" ~artificial_delay_s:(Some 2.0) ()
  in
  let fut_p1_swapped =
    Client.call client ~timeout_s:0.05 Trivial.Swapper.Client.swap p1
    |> Fut.reify_error
  in
  let p1_swapped =
    Fut.wait_block_exn fut_p1_swapped |> RPC.Error.result_of_fut_or_error
  in
  Fmt.printf "after timeout: %a@."
    (RPC.Error.pp_result Trivial.pp_pair)
    p1_swapped;
  assert (
    match p1_swapped with
    | Error err -> err.kind = RPC.Error_kind.Timeout
    | _ -> false);

  (* test out of order *)
  let p2_1 =
    Trivial.make_pair ~x:"hello" ~y:"world" ~artificial_delay_s:(Some 0.7) ()
  in
  let fut_p2_1 = Client.call client Trivial.Swapper.Client.swap p2_1 in

  let p2_2 =
    Trivial.make_pair ~x:"hello" ~y:"world" ~artificial_delay_s:(Some 0.05) ()
  in
  let fut_p2_2 = Client.call client Trivial.Swapper.Client.swap p2_2 in
  Trace.message "waiting for p2_2";
  let p2_2 = Fut.wait_block_exn fut_p2_2 in
  assert (not (Fut.is_done fut_p2_1));
  assert (p2_2 = p0_swapped);

  Trace.message "waiting for p2_1";
  let p2_1 = Fut.wait_block_exn fut_p2_1 in
  assert (p2_1 = p0_swapped);

  Fmt.printf "end tests@.";
  ()

let run_msg_test ~client () =
  let n = 80_000 in
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "run-msg.test" ~data:(fun () ->
        [ "n", `Int n ])
  in
  let msg = String.init n (fun i -> Char.chr (Char.code 'a' + (i mod 26))) in
  let msg = Trivial.make_big_string ~msg () in
  let count_fut = Client.call client Trivial.Swapper.Client.count_chars msg in
  let count = (Fut.wait_block_exn count_fut).count |> Int32.to_int in
  assert (count = n)

let run_stress_test ~client () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run-stress-test" in

  let p = Trivial.make_pair ~x:"a" ~y:"b" ~artificial_delay_s:None () in
  for _i = 1 to 1_000 do
    let batch_size = 50 in
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__ "run.batch" ~data:(fun () ->
          [ "i", `Int _i; "size", `Int batch_size ])
    in
    let futs : _ Fut.t list =
      List.init batch_size (fun _ ->
          Client.call client Trivial.Swapper.Client.swap p)
    in

    List.iter (fun f -> ignore (Fut.wait_block f : _ result)) futs
  done;
  Trace.message "stress test: done"

let run_client_stream_test ~client () : unit =
  let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "test-client-stream" in
  let push, res_fut =
    let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "start-client-stream" in
    Client.call_client_stream client Trivial.Beancount.Client.add_stream
  in

  for i = 1 to 10 do
    let@ _sp =
      Trace_core.with_span ~__FILE__ ~__LINE__ "send-stream" ~data:(fun () ->
          [ "i", `Int i ])
    in
    RPC.Push_stream.push push (Trivial.make_single_int ~i:(Int32.of_int i) ())
  done;
  Trace.message "closing stream";
  RPC.Push_stream.close push;
  Trace.message "closed stream";

  let res = Fut.wait_block_exn res_fut in
  Trace.messagef (fun k -> k "got result %ld" res.count);
  assert (res.count = 55l);
  ()

let run_server_stream_test ~client () : unit =
  let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "test-server-stream" in

  let fut : _ Fut.t =
    let@ _sp =
      Trace_core.with_span ~__FILE__ ~__LINE__ "test-server-stream.call"
    in
    Client.call_server_stream client Trivial.Beancount.Client.get_smaller_ints
      (Trivial.make_count ~count:10l ())
      ~init:(fun () -> ref [])
      ~timeout_s:2.
      ~on_item:(fun l (x : Trivial.single_int) -> l := Int32.to_int x.i :: !l)
      ~on_close:(fun l -> !l)
  in

  let l = Fut.wait_block fut in
  Result.iter_error
    (fun (exn, _) ->
      Fmt.printf "stream test failed with %s@." @@ Printexc.to_string exn)
    l;

  assert (l = Ok [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]);
  ()

let t_with_pipe () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.main.pipe" in

  let@ ic_client, oc_server = RPC.Util_pipe.with_pipe ~close_noerr:true () in
  let@ ic_server, oc_client = RPC.Util_pipe.with_pipe ~close_noerr:true () in

  let active = RPC.Simple_switch.create () in
  let@ runner = Moonpool.Ws_pool.with_ ~num_threads:4 () in

  let client : Client.t =
    Client.create ~active ~timer ~ic:ic_client ~oc:oc_client ()
  in
  let@ () =
    Fun.protect ~finally:(fun () ->
        let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "client.close" in
        Client.close_and_join client;
        Moonpool.Runner.shutdown runner)
  in

  (* thread for server *)
  let server : RPC.Server_for_client.t =
    let encoding = RPC.Encoding.read_from_ic ic_server in
    RPC.Server_for_client.create (* ~active *)
      ~runner ~timer ~services ~encoding ~ic:ic_server ~oc:oc_server ()
  in
  let@ () =
    Fun.protect ~finally:(fun () -> RPC.Server_for_client.close server)
  in

  run_tests_on ~client ();
  run_msg_test ~client ();

  Trace.message "shutting down";
  (* Switch.turn_off active; *)
  ()

let port = try Sys.getenv "TEST_PORT" |> int_of_string with _ -> 123456

let log_net_stats () =
  Trace.counter_int "net.sent" (RPC.Net_stats.get_n_sent ());
  Trace.counter_int "net.received" (RPC.Net_stats.get_n_received ())

let t_tcp () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.main.tcp" in

  let active = RPC.Simple_switch.create () in
  let@ runner = Moonpool.Ws_pool.with_ ~num_threads:4 () in

  let with_switch_off_if_fail f =
    try f ()
    with exn ->
      RPC.Switch.turn_off active;
      raise exn
  in

  if Trace.enabled () then RPC.Timer.run_every_s timer 0.05 log_net_stats;

  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let server : RPC.Tcp_server.t =
    RPC.Tcp_server.create ~active ~runner ~timer ~services addr
    |> RPC.Error.unwrap
  in

  (* background thread to accept connection *)
  let t_server = Thread.create RPC.Tcp_server.run server in
  Fmt.printf "listening on port %d@." port;
  let@ () =
    Fun.protect ~finally:(fun () ->
        Trace.message "join server";
        Thread.join t_server;
        Trace.message "joined server")
  in

  Trace.message "tcp: connecting...";

  let with_client f =
    let client : Client.t =
      RPC.Tcp_client.connect ~active ~runner ~timer addr |> RPC.Error.unwrap
    in
    let@ () =
      Fun.protect ~finally:(fun () ->
          Trace.message "closing client";
          let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "client.close" in
          Client.close_and_join client;
          Trace.message "closed client")
    in

    f client
  in

  let () =
    let@ () = with_switch_off_if_fail in
    let@ client = with_client in
    log_net_stats ();

    run_server_stream_test ~client ();
    log_net_stats ();

    run_tests_on ~client ();
    log_net_stats ();

    run_msg_test ~client ();
    log_net_stats ();

    run_client_stream_test ~client ();
    log_net_stats ();

    run_server_stream_test ~client ();
    log_net_stats ()
  in

  let t_stress1 =
    Moonpool.start_thread_on_some_domain
      (fun () ->
        Trace.set_thread_name "stress-test-1";
        let@ () = with_switch_off_if_fail in
        let@ client = with_client in
        run_stress_test ~client ())
      ()
  in
  let t_stress2 =
    Moonpool.start_thread_on_some_domain
      (fun () ->
        Trace.set_thread_name "stress-test-2";
        let@ () = with_switch_off_if_fail in
        let@ client = with_client in
        run_stress_test ~client ())
      ()
  in

  Thread.join t_stress1;
  Thread.join t_stress2;

  Trace.message "shutting down";
  RPC.Switch.turn_off active;
  ()

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.main" in

  try
    t_with_pipe ();
    t_tcp ();
    Trace.message "end main"
  with RPC.Error.E err ->
    Fmt.printf "error: %a@." RPC.Error.pp err;
    exit 1
