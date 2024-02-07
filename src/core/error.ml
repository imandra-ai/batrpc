(** Errors *)

open Common_

type kind = Error_kind.t =
  | Failure of string
  | Deser_error of string
  | Network_error of string
  | Rpc_error of Meta.error
  | Timeout
  | Other of exn * Printexc.raw_backtrace

type t = {
  kind: kind;
  ctx: t option;
}

let show (self : t) =
  let rec show_rec self =
    let k = Error_kind.show self.kind in
    match self.ctx with
    | None -> k
    | Some ctx -> spf "%s\n%s" k (show_rec ctx)
  in
  spf "Batrpc error:\n%s" (show_rec self)

let pp out self = Format.pp_print_string out (show self)
let[@inline] mk ?ctx kind : t = { kind; ctx }
let of_exn ~bt exn : t = { ctx = None; kind = Other (exn, bt) }
let wrap kind (e : t) : t = { e with ctx = Some { kind; ctx = e.ctx } }

exception E of t

let raise err = raise (E err)
let raise_err ?ctx kind : 'a = raise { kind; ctx }
let[@inline] fail ?ctx msg = raise { kind = Failure msg; ctx }
let[@inline] failf ?ctx fmt = Format.kasprintf (fail ?ctx) fmt

let guards msg f =
  match f () with
  | x -> x
  | exception E err -> raise @@ wrap (Failure msg) err
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    raise (wrap (Failure msg) @@ of_exn ~bt exn)

let guardf k f =
  match f () with
  | x -> x
  | exception E err ->
    k (fun fmt ->
        Format.kasprintf (fun msg -> raise @@ wrap (Failure msg) err) fmt)
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    k (fun fmt ->
        Format.kasprintf
          (fun msg -> raise @@ wrap (Failure msg) @@ of_exn ~bt exn)
          fmt)

type nonrec 'a result = ('a, t) result

let result_of_fut_or_error (f : 'a Fut.or_error) : 'a result =
  match f with
  | Ok x -> Ok x
  | Error (E err, _) -> Error err
  | Error (e, bt) -> Error (of_exn ~bt e)

let pp_result ppx out = function
  | Ok x -> ppx out x
  | Error err -> pp out err

let[@inline] unwrap = function
  | Ok x -> x
  | Error err -> raise err

let[@inline] unwrap_opt msg = function
  | Some x -> x
  | None -> fail msg

let try_with f : _ result =
  try Ok (f ()) with
  | E e -> Error e
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    let err = of_exn ~bt exn in
    Error err
