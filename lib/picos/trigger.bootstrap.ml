let[@inline never] error_awaiting () = invalid_arg "already awaiting"

type state =
  | Signaled
  | Awaiting : { action : t -> 'x -> 'y -> unit; x : 'x; y : 'y } -> state
  | Initial

and t = state Atomic.t

let create () = Atomic.make Initial
let is_signaled t = Atomic.get t == Signaled

let is_initial t =
  match Atomic.get t with
  | Initial -> true
  | Awaiting _ -> error_awaiting ()
  | Signaled -> false

let rec finish t ~allow_awaiting =
  match Atomic.get t with
  | Signaled -> ()
  | Awaiting r as before ->
      if allow_awaiting then
        if Atomic.compare_and_set t before Signaled then r.action t r.x r.y
        else finish t ~allow_awaiting
      else error_awaiting ()
  | Initial ->
      if not (Atomic.compare_and_set t Initial Signaled) then
        finish t ~allow_awaiting

let signal t = finish t ~allow_awaiting:true
let dispose t = finish t ~allow_awaiting:false

let rec on_signal t x y action =
  match Atomic.get t with
  | Signaled -> false
  | Awaiting _ -> error_awaiting ()
  | Initial ->
      let success =
        Atomic.compare_and_set t Initial (Awaiting { action; x; y })
      in
      if success then success else on_signal t x y action

let from_action x y action = Atomic.make (Awaiting { action; x; y })
