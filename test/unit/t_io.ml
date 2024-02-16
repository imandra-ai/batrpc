open Batrpc_core

let str = "hello\n world\n\n123"

let () =
  let ic = new Io.In.bufferized @@ new Io.In.of_str str in
  let lines = Io.In.read_lines ic in
  Printf.printf "lines:\n";
  List.iter (Printf.printf "- %S\n") lines;
  flush stdout
