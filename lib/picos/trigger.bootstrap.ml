let[@inline never] error_awaiting _ = invalid_arg "already awaiting"

type state =
  | Signaled
  | Awaiting : { action : t -> 'x -> 'y -> unit; x : 'x; y : 'y } -> state
  | Initial

and t = state Atomic.t

let finish t ~allow_awaiting =
  match Atomic.get t with
  | Signaled -> ()
  | Awaiting r as before ->
      if allow_awaiting then begin
        if Atomic.compare_and_set t before Signaled then r.action t r.x r.y
      end
      else error_awaiting before
  | Initial ->
      if not (Atomic.compare_and_set t Initial Signaled) then begin
        match Atomic.get t with
        | Signaled | Initial -> ()
        | Awaiting r as before ->
            if allow_awaiting && Atomic.compare_and_set t before Signaled then
              r.action t r.x r.y
      end

let on_signal t x y action =
  match Atomic.get t with
  | Signaled -> false
  | Awaiting _ as any -> error_awaiting any
  | Initial -> begin
      let success =
        Atomic.compare_and_set t Initial (Awaiting { action; x; y })
      in
      if success then success
      else
        match Atomic.get t with Signaled -> false | any -> error_awaiting any
    end

let[@inline] create () = Atomic.make Initial

let[@inline] is_initial t =
  match Atomic.get t with
  | Initial -> true
  | Awaiting _ as any -> error_awaiting any
  | Signaled -> false

let[@inline] from_action x y action = Atomic.make (Awaiting { action; x; y })
let[@inline] is_signaled t = Atomic.get t == Signaled
let[@inline] signal t = finish t ~allow_awaiting:true
let[@inline] dispose t = finish t ~allow_awaiting:false
