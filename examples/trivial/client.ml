let ( let@ ) = ( @@ )
let port = 12345

module RPC = Batrpc
module Client = RPC.Client
module Fut = Moonpool.Fut

let () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let timer = Timer.create () in

  Printf.printf "connecting...\n%!";
  let client : Client.t =
    match RPC.Tcp_client.connect ~timer addr with
    | Ok c -> c
    | Error err ->
      let err =
        Error.add_ctx
          (Error.messagef "RPC: connecting to %a" Util_sockaddr.pp addr)
          err
      in
      Error.raise_err err
  in
  let@ () = Fun.protect ~finally:(fun () -> Client.close_and_join client) in

  let pair = Trivial.make_pair ~x:"hello" ~y:"world" () in
  Format.printf "pair: %a@." Trivial.pp_pair pair;

  let fut_pair_swapped : Trivial.pair Moonpool.Fut.t =
    Client.call client ~timeout_s:2. Trivial.Swapper.Client.swap pair
  in

  (* the request is in-flight, we can do other things here … *)

  (* now wait for the result *)
  let pair_swapped = Fut.wait_block_exn fut_pair_swapped in
  Format.printf "swapped pair: %a@." Trivial.pp_pair pair_swapped;
  ()
