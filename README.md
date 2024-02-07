# BatRPC

[![Build and Test](https://github.com/imandra-ai/batrpc/actions/workflows/main.yml/badge.svg)](https://github.com/imandra-ai/batrpc/actions/workflows/main.yml)

This is a RPC framework for OCaml, based on protobuf as a wire format.

## Overview

The goal of BatRPC is to provide an efficient and flexible RPC system for
our needs at Imandra. It is designed for long-lived connections between
two processes that can both act as a server and a client.

[Protobuf](https://protobuf.dev) is used as an
[IDL](https://en.wikipedia.org/wiki/Interface_description_language) to
describe the types used for communication, as well as the actual
RPC endpoints.
Protobuf also generates (de)serialization code for these types, and
bundles for the services.

Each side of a connection can emit requests to the other side (acting
as a "client"), and host services that receive requests from the
other side (acting as "server").
Multiple services can be provided on a single connection, provided
they have distinct names.

## Features

- auto-generation of types, services, and (de)serialization using [ocaml-protoc](https://github.com/mransan/ocaml-protoc/)
- basic per-message compression for large messages, using `deflate`. Stream-level
    compression is not supported by BatRPC, but could be implemented transparently:
    a `Rpc_conn.t` takes a pair of input/output byte streams which could be
    compressed or encrypted.
- messages carry headers, ie pairs of strings, pretty much like HTTP headers.
- middlewares on the server side. A middleware can take an incoming request
    and its future reply, and insert metadata in headers, perform logging, tracing, etc.
- baked-on concurrency using [moonpool](https://github.com/c-cube/moonpool/) as a
    thread pool and future library.
- kinds of requests:
    * [x] simple request/response
    * [x] client-side streaming (the client sends a stream of values)
    * [x] server-side streaming (the server returns a stream of values)
    * [ ] bidirectional streaming

## Example

<details>
<summary>A basic example, fully worked out</summary>

The code is in `examples/trivial`.

Given this file (see `examples/trivial/trivial.proto`):

```proto
message Pair {
  string x = 1;
  string y = 2;
}

message BigString {
  string msg = 1;
}

message Count {
  int32 count = 1;
}

message SingleInt {
  int32 i = 0;
}

service Swapper {
  rpc swap(Pair) returns (Pair);
  rpc count_chars(BigString) returns (Count);
}
```

and the dune rules

```scheme
(rule
 (targets trivial.ml trivial.mli)
 (deps trivial.proto)
 (mode promote)
 (action
  (run ocaml-protoc --binary --pp --yojson --services --make --ml_out ./ %{deps})))
```

We get files `trivial.ml` and `trivial.mli`. The signature generated from this is, roughly:

```trivial.mli
type pair = {
  x : string;
  y : string;
  artificial_delay_s : float option;
}

type big_string = {
  msg : string;
}

type count = {
  count : int32;
}

type single_int = {
  i : int32;
}

val pp_pair : Format.formatter -> pair -> unit 
(* … *)


val encode_pb_pair : pair -> Pbrt.Encoder.t -> unit
(* … *)

val decode_pb_pair : Pbrt.Decoder.t -> pair
(* … *)


(** Swapper service *)
module Swapper : sig
  open Pbrt_services
  open Pbrt_services.Value_mode
  
  module Client : sig
    
    val swap : (pair, unary, pair, unary) Client.rpc
    
    val count_chars : (big_string, unary, count, unary) Client.rpc
  end
  
  module Server : sig
    (** Produce a server implementation from handlers *)
    val make : 
      swap:((pair, unary, pair, unary) Server.rpc -> 'handler) ->
      count_chars:((big_string, unary, count, unary) Server.rpc -> 'handler) ->
      unit -> 'handler Pbrt_services.Server.t
  end
end
```

We can then use the `batrpc` library and this generated code, together, to
implement RPC clients and servers.
Here "client" and "server" really means "network client" and "network server"
(ie clients are the ones opening connections to servers); from the RPC
point of view, once the connection is established, both ends act both are
client and server in the sense that they can provide services, and emit
requests to services.

### Client side

Let's write a TCP client.

```ocaml
let (let@) = (@@)
let port = 12345

module RPC = Batrpc
module Client = RPC.Basic_client
module Fut = Moonpool.Fut

let () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let timer = RPC.Simple_timer.create () in

  Printf.printf "connecting...\n%!";
  let client : Client.t =
    RPC.Tcp_client.connect ~timer addr |> RPC.Error.unwrap
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
```


### Server side

```ocaml
let ( let@ ) = ( @@ )
let port = 12345

module RPC = Batrpc
module Fut = Moonpool.Fut

(* this is where we implement the actual logic for the services *)

let trivial_service =
  Trivial.Swapper.Server.make
    ~swap:(fun rpc ->
      RPC.mk_handler rpc @@ fun (p : Trivial.pair) ->
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "test.swap" in
      Fut.return @@ Trivial.make_pair ~x:p.y ~y:p.x ())
    ~count_chars:(fun rpc ->
      RPC.mk_handler rpc @@ fun (msg : Trivial.big_string) ->
      let n = String.length msg.msg in
      Fut.return @@ Trivial.make_count ~count:(Int32.of_int n) ())
    ()

(* we could host multiple services, here we only have one *)
let services = [ trivial_service ]

let () =
  let active = RPC.Simple_switch.create () in
  let timer = RPC.Simple_timer.create () in

  (* we need a thread pool to run the tasks *)
  let@ runner = Moonpool.Ws_pool.with_ ~num_threads:8 () in

  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let server : RPC.Tcp_server.t =
    RPC.Tcp_server.create ~active ~runner ~timer ~services addr
    |> RPC.Error.unwrap
  in

  (* background thread to accept connection *)
  Format.eprintf "listening on port %d@." port;
  RPC.Tcp_server.run server
```


</details>
