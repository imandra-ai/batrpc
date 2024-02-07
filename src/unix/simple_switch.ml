include Switch

type cb = unit -> unit

type state =
  | On of {
      n: int;
      l: cb list;
    }
  | Off

type t_ = { st: state Atomic.t } [@@unboxed]

let update_ (type a) (self : t_) (f : state -> a * state) : a =
  let rec loop () =
    let old_st = Atomic.get self.st in
    let x, new_st = f old_st in
    if old_st == new_st || Atomic.compare_and_set self.st old_st new_st then
      x
    else
      (loop [@tailcall]) ()
  in
  loop ()

let on_turn_off_ (self : t_) (f : cb) : unit =
  let must_fire =
    update_ self (function
      | Off -> true, Off
      | On r -> false, On { r with l = f :: r.l })
  in
  if must_fire then (* call now *) f ()

let turn_off_ (self : t_) () : unit =
  match Atomic.exchange self.st Off with
  | Off -> ()
  | On { l; n = _ } -> List.iter (fun f -> f ()) l

let[@inline] is_on_ (self : t_) () : bool =
  match Atomic.get self.st with
  | On _ -> true
  | Off -> false

let create () : t =
  let self = { st = Atomic.make (On { l = []; n = 0 }) } in
  {
    is_on = is_on_ self;
    on_turn_off = on_turn_off_ self;
    turn_off = turn_off_ self;
  }
