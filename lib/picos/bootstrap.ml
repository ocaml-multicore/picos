module Trigger = struct
  type state =
    | Signaled
    | Awaiting : ([ `On | `Signal ] t -> 'x -> 'y -> unit) * 'x * 'y -> state
    | Initial

  and 'allowed t = state Atomic.t

  let create () = Atomic.make Initial

  let rec signal t =
    match Atomic.get t with
    | Signaled -> ()
    | Initial ->
        if not (Atomic.compare_and_set t Initial Signaled) then signal t
    | Awaiting (action, x, y) as before ->
        if Atomic.compare_and_set t before Signaled then action t x y
        else signal t

  let is_signaled t = Atomic.get t == Signaled
end

module Computation = struct
  type 'a state =
    | Canceled of Exn_bt.t
    | Returned of 'a
    | Continue of { balance : int; triggers : [ `Signal ] Trigger.t list }

  type ('a, 'allowed) t = 'a state Atomic.t

  let create () = Atomic.make (Continue { balance = 0; triggers = [] })

  let canceled t =
    match Atomic.get t with
    | Canceled exn_bt -> Some exn_bt
    | Returned _ | Continue _ -> None

  open struct
    (** [gc] reverses the list of triggers while dropping signaled triggers.
        This should be fine (it doesn't make the behavior non-deterministic, for
        example), but it might make sense to take extra steps to preserve the
        original ordering. *)
    let rec gc length triggers = function
      | [] -> Continue { balance = length; triggers }
      | r :: rs ->
          if Trigger.is_signaled r then gc length triggers rs
          else gc (length + 1) (r :: triggers) rs
  end

  let rec try_attach backoff t trigger =
    match Atomic.get t with
    | Returned _ | Canceled _ -> false
    | Continue r as before ->
        let after =
          if 0 <= r.balance then
            Continue
              { balance = r.balance + 1; triggers = trigger :: r.triggers }
          else gc 1 [ trigger ] r.triggers
        in
        Atomic.compare_and_set t before after
        || try_attach (Backoff.once backoff) t trigger

  let try_attach t trigger = try_attach Backoff.default t trigger

  let rec detach backoff t =
    match Atomic.get t with
    | Returned _ | Canceled _ -> ()
    | Continue r as before ->
        let after =
          if 0 <= r.balance then Continue { r with balance = r.balance - 2 }
          else gc 0 [] r.triggers
        in
        if not (Atomic.compare_and_set t before after) then
          detach (Backoff.once backoff) t

  let detach t trigger =
    Trigger.signal trigger;
    detach Backoff.default t
end

module Fiber = struct
  type -'allowed t =
    | Fiber : {
        computation : ('a, [ `Await | `Cancel ]) Computation.t;
        mutable forbid : bool;
        mutable fls : Obj.t array;
      }
        -> 'allowed t

  let create ~forbid computation =
    Fiber { computation :> _; forbid; fls = [||] }

  let has_forbidden (Fiber r) = r.forbid

  let canceled (Fiber r) =
    if r.forbid then None else Computation.canceled r.computation

  let try_attach (Fiber r) trigger =
    Computation.try_attach r.computation trigger

  let detach (Fiber r) trigger = Computation.detach r.computation trigger
end
