open Common_

type header = Meta.header

type ctx = {
  hmap: Hmap.t;
  headers: header list;
}

let empty_ctx : ctx = { hmap = Hmap.empty; headers = [] }

type 'a with_ctx = ctx * 'a

type ('req, 'res) t = 'req with_ctx -> 'res with_ctx Fut.t
(** A basic request handler. *)
