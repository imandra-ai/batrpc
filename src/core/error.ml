(** Errors *)

open Common_

type kind =
  | Failure of string
  | Deser_error of string
  | Network_error of string

type t = {
  kind: kind;
  ctx: t option;
}

let pp_kind out = function
  | Deser_error e -> Format.fprintf out "deserialization error: %s" e
  | Network_error e -> Format.fprintf out "network error: %s" e
  | Failure f -> Format.fprintf out "%s" f

let pp out (self : t) =
  let rec pp out self =
    pp_kind out self.kind;
    match self.ctx with
    | None -> ()
    | Some ctx -> Format.fprintf out "@,%a" pp ctx
  in
  Format.fprintf out "@[<v2>Batrpc error:@ %a@]" pp self

let[@inline] mk ?ctx kind : t = { kind; ctx }

exception E of t

let[@inline] fail ?ctx msg = raise (E { kind = Failure msg; ctx })
let[@inline] failf ?ctx fmt = Format.kasprintf (fail ?ctx) fmt

let guards msg f =
  match f () with
  | x -> x
  | exception E err -> fail ~ctx:err msg

let guardf k f =
  match f () with
  | x -> x
  | exception E err -> k (fun fmt -> Format.kasprintf (fail ~ctx:err) fmt)

type 'a or_error = ('a, t) result

let try_with f : _ or_error = try Ok (f ()) with E e -> Error e
