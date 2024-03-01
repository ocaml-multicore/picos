module Trigger = struct
  type state =
    | Signaled
    | Awaiting : (t -> 'x -> 'y -> unit) * 'x * 'y -> state
    | Initial

  and t = state Atomic.t

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
  let is_initial t = Atomic.get t == Initial
  let[@inline never] awaiting () = invalid_arg "Trigger: already awaiting"

  let rec on_signal t x y action =
    match Atomic.get t with
    | Initial ->
        Atomic.compare_and_set t Initial (Awaiting (action, x, y))
        || on_signal t x y action
    | Signaled -> false
    | Awaiting _ -> awaiting ()
end

module Computation = struct
  type 'a state =
    | Canceled of Exn_bt.t
    | Returned of 'a
    | Continue of { balance : int; triggers : Trigger.t list }

  type 'a t = 'a state Atomic.t

  let create () = Atomic.make (Continue { balance = 0; triggers = [] })

  let canceled t =
    match Atomic.get t with
    | Canceled exn_bt -> Some exn_bt
    | Returned _ | Continue _ -> None

  open struct
    (** [gc] is called when balance becomes negative by both [try_attach] and
        [detach].  This ensures that the [O(n)] lazy removal done by [gc] cannot
        cause starvation, because the only reason that CAS fails after [gc] is
        that someone else completed the [gc]. *)
    let rec gc balance triggers = function
      | [] ->
          let triggers = if balance <= 1 then triggers else List.rev triggers in
          Continue { balance; triggers }
      | r :: rs ->
          if Trigger.is_signaled r then gc balance triggers rs
          else gc (balance + 1) (r :: triggers) rs
  end

  let rec try_attach t trigger backoff =
    match Atomic.get t with
    | Returned _ | Canceled _ -> false
    | Continue r as before ->
        (* We check the trigger before potential allocations. *)
        (not (Trigger.is_signaled trigger))
        &&
        let after =
          if 0 <= r.balance then
            Continue
              { balance = r.balance + 1; triggers = trigger :: r.triggers }
          else gc 1 [ trigger ] r.triggers
        in
        Atomic.compare_and_set t before after
        || try_attach t trigger (Backoff.once backoff)

  let try_attach t trigger = try_attach t trigger Backoff.default

  let rec detach t backoff =
    match Atomic.get t with
    | Returned _ | Canceled _ -> ()
    | Continue r as before ->
        let after =
          if 0 <= r.balance then Continue { r with balance = r.balance - 2 }
          else gc 0 [] r.triggers
        in
        if not (Atomic.compare_and_set t before after) then
          detach t (Backoff.once backoff)

  let detach t trigger =
    Trigger.signal trigger;
    detach t Backoff.default

  type packed = Packed : 'a t -> packed

  let is_running t =
    match Atomic.get t with
    | Canceled _ | Returned _ -> false
    | Continue _ -> true

  open struct
    let rec try_terminate t after backoff =
      match Atomic.get t with
      | Returned _ | Canceled _ -> false
      | Continue r as before ->
          if Atomic.compare_and_set t before after then begin
            List.iter Trigger.signal r.triggers;
            true
          end
          else try_terminate t after (Backoff.once backoff)
  end

  let returned_unit = Returned ()
  let finished = Atomic.make returned_unit
  let try_return t value = try_terminate t (Returned value) Backoff.default
  let try_finish t = try_terminate t returned_unit Backoff.default
  let try_cancel t exn_bt = try_terminate t (Canceled exn_bt) Backoff.default
  let return t value = try_return t value |> ignore
  let finish t = try_finish t |> ignore
  let cancel t exn_bt = try_cancel t exn_bt |> ignore

  let try_capture t fn x =
    match fn x with
    | y -> try_return t y
    | exception exn -> try_cancel t (Exn_bt.get exn)

  let capture t fn x = try_capture t fn x |> ignore

  let check t =
    match Atomic.get t with
    | Canceled exn_bt -> Exn_bt.raise exn_bt
    | Returned _ | Continue _ -> ()

  let peek t =
    match Atomic.get t with
    | Canceled exn_bt -> Some (Error exn_bt)
    | Returned value -> Some (Ok value)
    | Continue _ -> None

  open struct
    let propagate _ from into =
      match canceled from with None -> () | Some exn_bt -> cancel into exn_bt
  end

  let canceler ~from ~into =
    Atomic.make (Trigger.Awaiting (propagate, from, into))
end

module Fiber = struct
  type t =
    | Fiber : {
        computation : 'a Computation.t;
        mutable forbid : bool;
        mutable fls : Obj.t array;
      }
        -> t

  let create ~forbid computation = Fiber { computation; forbid; fls = [||] }
  let has_forbidden (Fiber r) = r.forbid

  let canceled (Fiber r) =
    if r.forbid then None else Computation.canceled r.computation

  let try_attach (Fiber r) trigger =
    Computation.try_attach r.computation trigger

  let detach (Fiber r) trigger = Computation.detach r.computation trigger
  let[@inline] equal t1 t2 = t1 == t2
  let computation (Fiber r) = Computation.Packed r.computation
  let check (Fiber r) = if not r.forbid then Computation.check r.computation

  open struct
    let explicitly ~forbid (Fiber r) body =
      if r.forbid = forbid then body ()
      else
        match body (r.forbid <- forbid) with
        | value ->
            r.forbid <- not forbid;
            value
        | exception exn ->
            r.forbid <- not forbid;
            raise exn
  end

  let forbid t body = explicitly ~forbid:true t body
  let permit t body = explicitly ~forbid:false t body

  module FLS = struct
    type 'a key = { index : int; default : Obj.t; compute : unit -> 'a }

    open struct
      let compute () = failwith "impossible"
      let counter = Atomic.make 0
      let[@inline] unique () = Obj.repr counter

      let ceil_pow_2_minus_1 n =
        let n = n lor (n lsr 1) in
        let n = n lor (n lsr 2) in
        let n = n lor (n lsr 4) in
        let n = n lor (n lsr 8) in
        let n = n lor (n lsr 16) in
        if Sys.int_size > 32 then n lor (n lsr 32) else n

      let grow old_fls i =
        let new_length = ceil_pow_2_minus_1 (i + 1) in
        let new_fls = Array.make new_length (unique ()) in
        Array.blit old_fls 0 new_fls 0 (Array.length old_fls);
        new_fls
    end

    type 'a initial = Constant of 'a | Computed of (unit -> 'a)

    let new_key initial =
      let index = Atomic.fetch_and_add counter 1 in
      match initial with
      | Constant default -> { index; default = Obj.repr default; compute }
      | Computed compute -> { index; default = unique (); compute }

    let get (Fiber r) key =
      let fls = r.fls in
      if key.index < Array.length fls then begin
        let value = Array.unsafe_get fls key.index in
        if value != unique () then Obj.magic value
        else
          let value = key.default in
          if value != unique () then begin
            (* As the [fls] array was already large enough, we cache the default
               value in the array. *)
            Array.unsafe_set fls key.index
              (Sys.opaque_identity (Obj.repr value));
            Obj.magic value
          end
          else
            let value = key.compute () in
            Array.unsafe_set fls key.index
              (Sys.opaque_identity (Obj.repr value));
            value
      end
      else
        let value = key.default in
        if value != unique () then Obj.magic value
        else
          let value = key.compute () in
          let fls = grow fls key.index in
          r.fls <- fls;
          Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value));
          value

    let set (Fiber r) key value =
      let fls = r.fls in
      if key.index < Array.length fls then
        Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value))
      else
        let fls = grow fls key.index in
        r.fls <- fls;
        Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value))
  end
end
