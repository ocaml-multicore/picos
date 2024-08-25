let[@inline never] impossible () = failwith "impossible"

module Exn_bt = Picos_exn_bt

module Trigger = struct
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
        Atomic.compare_and_set t Initial (Awaiting { action; x; y })
        || on_signal t x y action

  let from_action x y action = Atomic.make (Awaiting { action; x; y })
end

module Computation = struct
  let[@inline never] error_negative_or_nan () =
    invalid_arg "seconds must be non-negative"

  let[@inline never] error_returned () = invalid_arg "already returned"

  type tx_state = Stopped  (** [= false] *) | Started | Aborted

  type _ tx =
    | Stopped : [> `Stopped ] tx
    | Running : {
        state : tx_state Atomic.t;
        mutable completions : [ `Nil | `Completion ] completions;
      }
        -> [> `Running ] tx

  and _ completions =
    | Unused : [> `Unused ] completions  (** Affects [Nil]s value! *)
    | Nil : [> `Nil ] completions  (** [= true] *)
    | Completion : {
        computation : 'a t;
        before : ('a, [ `Continue ]) st;
        completions : [ `Nil | `Completion ] completions;
      }
        -> [> `Completion ] completions

  and ('a, _) st =
    | Canceled : {
        exn_bt : Exn_bt.t;
        mutable tx : [ `Stopped | `Running ] tx;
      }
        -> ('a, [> `Canceled ]) st
    | Returned : {
        value : 'a;
        mutable tx : [ `Stopped | `Running ] tx;
      }
        -> ('a, [> `Returned ]) st
    | Continue : {
        balance_and_mode : int;
        triggers : Trigger.t list;
      }
        -> ('a, [> `Continue ]) st

  and 'a state =
    | S : ('a, [< `Canceled | `Returned | `Continue ]) st -> 'a state
  [@@unboxed]

  and 'a t = 'a state Atomic.t

  let fifo_bit = 1
  let one = 2

  let[@inline] signal (Continue r : (_, [ `Continue ]) st) =
    List.iter Trigger.signal
      (if r.balance_and_mode land fifo_bit <> fifo_bit then r.triggers
       else List.rev r.triggers)

  module Tx = struct
    let[@inline never] already_committed () = invalid_arg "already committed"
    let[@inline] same (type a b) (x : a t) (y : b t) = x == (Obj.magic y : a t)

    type t = [ `Running ] tx

    let[@inline] create () =
      Running { state = Atomic.make Started; completions = Nil }

    let rec rollback tx = function
      | Nil -> true
      | Completion r -> begin
          begin
            match Atomic.get r.computation with
            | ( S (Canceled { tx = previous_tx; _ })
              | S (Returned { tx = previous_tx; _ }) ) as before ->
                if tx == previous_tx then
                  Atomic.compare_and_set r.computation before (S r.before)
                  |> ignore
            | S (Continue _) -> ()
          end;
          rollback tx r.completions
        end

    let rec abort (Running r as tx : [ `Running ] tx) =
      match Atomic.get r.state with
      | Started ->
          if Atomic.compare_and_set r.state Started Aborted then
            rollback tx r.completions
          else abort tx (* state is write once so no need to backoff *)
      | Aborted -> rollback tx r.completions
      | Stopped -> false

    let[@inline] try_abort = function
      | Stopped -> false
      | Running _ as tx -> abort tx

    let rec try_complete (Running r as tx : [ `Running ] tx) computation backoff
        (after : (_, [< `Canceled | `Returned ]) st) =
      match Atomic.get computation with
      | S (Continue _ as before) ->
          Atomic.get r.state == Started
          &&
          let completions = r.completions in
          r.completions <- Completion { computation; before; completions };
          Atomic.compare_and_set computation (S before) (S after)
          || begin
               r.completions <- completions;
               try_complete tx computation (Backoff.once backoff) after
             end
      | S (Canceled { tx = previous_tx; _ })
      | S (Returned { tx = previous_tx; _ }) ->
          if try_abort previous_tx then
            try_complete tx computation backoff after
          else (not (abort tx)) && already_committed ()

    let rec commit = function
      | Nil -> true
      | Completion r ->
          begin
            match Atomic.get r.computation with
            | S (Canceled r) -> r.tx <- Stopped
            | S (Returned r) -> r.tx <- Stopped
            | S (Continue _) -> impossible ()
          end;
          signal r.before;
          commit r.completions

    let try_commit (Running r : [ `Running ] tx) =
      Atomic.compare_and_set r.state Started Stopped && commit r.completions

    let[@inline] try_return (Running _ as tx : [ `Running ] tx) computation
        value =
      try_complete tx computation Backoff.default (Returned { value; tx })

    let[@inline] try_cancel (Running _ as tx : [ `Running ] tx) computation
        exn_bt =
      try_complete tx computation Backoff.default (Canceled { exn_bt; tx })
  end

  let empty_fifo = S (Continue { triggers = []; balance_and_mode = fifo_bit })
  and empty_lifo = S (Continue { triggers = []; balance_and_mode = 0 })

  let create ?(mode : [ `FIFO | `LIFO ] = `FIFO) () =
    Atomic.make (if mode == `FIFO then empty_fifo else empty_lifo)

  let with_action ?(mode : [ `FIFO | `LIFO ] = `FIFO) x y action =
    let balance_and_mode = one + Bool.to_int (mode == `FIFO) in
    let trigger = Trigger.from_action x y action in
    Atomic.make (S (Continue { balance_and_mode; triggers = [ trigger ] }))

  let is_canceled t =
    match Atomic.get t with
    | S (Canceled { tx; _ }) -> tx == Stopped
    | S (Returned _) | S (Continue _) -> false

  let canceled t =
    match Atomic.get t with
    | S (Canceled { exn_bt; tx }) -> if tx == Stopped then Some exn_bt else None
    | S (Returned _) | S (Continue _) -> None

  (** [gc] is called when balance becomes negative by both [try_attach] and
      [detach].  This ensures that the [O(n)] lazy removal done by [gc] cannot
      cause starvation, because the only reason that CAS fails after [gc] is
      that someone else completed the [gc]. *)
  let rec gc balance_and_mode triggers = function
    | [] ->
        if balance_and_mode <= fifo_bit then
          if balance_and_mode == fifo_bit then empty_fifo else empty_lifo
        else
          let triggers =
            if balance_and_mode <= one + fifo_bit then triggers
            else List.rev triggers
          in
          S (Continue { balance_and_mode; triggers })
    | r :: rs ->
        if Trigger.is_signaled r then gc balance_and_mode triggers rs
        else gc (balance_and_mode + one) (r :: triggers) rs

  let rec try_attach t trigger backoff =
    match Atomic.get t with
    | S (Returned { tx; _ }) | S (Canceled { tx; _ }) ->
        Tx.try_abort tx && try_attach t trigger backoff
    | S (Continue r) as before ->
        (* We check the trigger before potential allocations. *)
        (not (Trigger.is_signaled trigger))
        &&
        let after =
          if fifo_bit <= r.balance_and_mode then
            let balance_and_mode = r.balance_and_mode + one in
            let triggers = trigger :: r.triggers in
            S (Continue { balance_and_mode; triggers })
          else
            gc (one + (r.balance_and_mode land fifo_bit)) [ trigger ] r.triggers
        in
        Atomic.compare_and_set t before after
        || try_attach t trigger (Backoff.once backoff)

  let try_attach t trigger = try_attach t trigger Backoff.default

  let rec unsafe_unsuspend t backoff =
    match Atomic.get t with
    | S (Returned { tx; _ }) ->
        if Tx.try_abort tx then unsafe_unsuspend t backoff else true
    | S (Canceled { tx; _ }) ->
        if Tx.try_abort tx then unsafe_unsuspend t backoff else false
    | S (Continue r) as before ->
        let after =
          if fifo_bit <= r.balance_and_mode then
            let balance_and_mode = r.balance_and_mode - (2 * one) in
            S (Continue { r with balance_and_mode })
          else gc (r.balance_and_mode land fifo_bit) [] r.triggers
        in
        Atomic.compare_and_set t before after
        || unsafe_unsuspend t (Backoff.once backoff)

  let detach t trigger =
    Trigger.signal trigger;
    unsafe_unsuspend t Backoff.default |> ignore

  (** This cannot be [@@unboxed] because [Atomic.t] is opaque *)
  type packed = Packed : 'a t -> packed

  let is_running t =
    match Atomic.get t with
    | S (Canceled { tx; _ }) | S (Returned { tx; _ }) -> tx != Stopped
    | S (Continue _) -> true

  let rec try_terminate t after backoff =
    match Atomic.get t with
    | S (Returned { tx; _ }) | S (Canceled { tx; _ }) ->
        if Tx.try_abort tx then try_terminate t after backoff else false
    | S (Continue _ as before) ->
        if Atomic.compare_and_set t (S before) after then begin
          signal before;
          true
        end
        else try_terminate t after (Backoff.once backoff)

  let returned_unit = Obj.magic (S (Returned { value = (); tx = Stopped }))

  let[@inline] make_returned value =
    if value == Obj.magic () then returned_unit
    else S (Returned { value; tx = Stopped })

  let returned value = Atomic.make (make_returned value)
  let finished = Atomic.make (make_returned ())
  let try_return t value = try_terminate t (make_returned value) Backoff.default
  let try_finish t = try_terminate t returned_unit Backoff.default

  let try_cancel t exn_bt =
    try_terminate t (S (Canceled { exn_bt; tx = Stopped })) Backoff.default

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
    | S (Canceled { exn_bt; tx; _ }) ->
        if tx == Stopped then Exn_bt.raise exn_bt
    | S (Returned _) | S (Continue _) -> ()

  let peek t =
    match Atomic.get t with
    | S (Canceled { exn_bt; tx; _ }) ->
        if tx == Stopped then Some (Error exn_bt) else None
    | S (Returned { value; tx; _ }) ->
        if tx == Stopped then Some (Ok value) else None
    | S (Continue _) -> None

  let propagate _ from into =
    match Atomic.get from with
    | S (Returned _) | S (Continue _) -> ()
    | S (Canceled _ as after) ->
        try_terminate into (S after) Backoff.default |> ignore

  let canceler ~from ~into = Trigger.from_action from into propagate

  let check_non_negative seconds =
    if not (0.0 <= seconds) then error_negative_or_nan ()

  let rec get_or block t =
    match Atomic.get t with
    | S (Returned { value; tx; _ }) ->
        if tx == Stopped then value else get_or block (block t)
    | S (Canceled { exn_bt; tx; _ }) ->
        if tx == Stopped then Exn_bt.raise exn_bt else get_or block (block t)
    | S (Continue _) -> get_or block (block t)

  let attach_canceler ~from ~into =
    let canceler = canceler ~from ~into in
    if try_attach from canceler then canceler
    else begin
      check from;
      error_returned ()
    end
end

module Fiber = struct
  type non_float = [ `Non_float of non_float ]

  type _ tdt =
    | Nothing : [> `Nothing ] tdt
    | Fiber : {
        mutable forbid : bool;
        mutable packed : Computation.packed;
        mutable fls : non_float array;
      }
        -> [> `Fiber ] tdt

  type t = [ `Fiber ] tdt

  let create_packed ~forbid packed = Fiber { forbid; packed; fls = [||] }

  let create ~forbid computation =
    create_packed ~forbid (Computation.Packed computation)

  let has_forbidden (Fiber r : t) = r.forbid

  let is_canceled (Fiber r : t) =
    (not r.forbid)
    &&
    let (Packed computation) = r.packed in
    Computation.is_canceled computation

  let canceled (Fiber r : t) =
    if r.forbid then None
    else
      let (Packed computation) = r.packed in
      Computation.canceled computation

  let get_computation (Fiber r : t) = r.packed
  let set_computation (Fiber r : t) packed = r.packed <- packed

  let check (Fiber r : t) =
    if not r.forbid then
      let (Packed computation) = r.packed in
      Computation.check computation

  let[@inline] equal t1 t2 = t1 == t2

  let exchange (Fiber r : t) ~forbid =
    let before = r.forbid in
    r.forbid <- forbid;
    before

  let set (Fiber r : t) ~forbid = r.forbid <- forbid

  let explicitly (Fiber r : t) body ~forbid =
    if r.forbid = forbid then body ()
    else
      match body (r.forbid <- forbid) with
      | value ->
          r.forbid <- not forbid;
          value
      | exception exn ->
          r.forbid <- not forbid;
          raise exn

  let forbid t body = explicitly t body ~forbid:true
  let permit t body = explicitly t body ~forbid:false

  let try_suspend (Fiber r : t) trigger x y resume =
    let (Packed computation) = r.packed in
    if not r.forbid then begin
      if Computation.try_attach computation trigger then
        Trigger.on_signal trigger x y resume
        || begin
             Computation.detach computation trigger;
             false
           end
      else if Computation.is_canceled computation then begin
        Trigger.dispose trigger;
        false
      end
      else Trigger.on_signal trigger x y resume
    end
    else Trigger.on_signal trigger x y resume

  let[@inline] unsuspend (Fiber r : t) trigger =
    assert (Trigger.is_signaled trigger);
    r.forbid
    ||
    let (Packed computation) = r.packed in
    Computation.unsafe_unsuspend computation Backoff.default

  module FLS = struct
    type fiber = t
    type 'a t = int

    let counter = Atomic.make 0
    let unique = Sys.opaque_identity (Obj.magic counter : non_float)

    let grow old_fls i =
      let ceil_pow_2_minus_1 n =
        let n = Nativeint.of_int n in
        let n = Nativeint.logor n (Nativeint.shift_right_logical n 1) in
        let n = Nativeint.logor n (Nativeint.shift_right_logical n 2) in
        let n = Nativeint.logor n (Nativeint.shift_right_logical n 4) in
        let n = Nativeint.logor n (Nativeint.shift_right_logical n 8) in
        let n = Nativeint.logor n (Nativeint.shift_right_logical n 16) in
        Nativeint.to_int
          (if Sys.int_size > 32 then
             Nativeint.logor n (Nativeint.shift_right_logical n 32)
           else n)
      in
      let new_length = ceil_pow_2_minus_1 (i + 1) in
      let new_fls = Array.make new_length unique in
      Array.blit old_fls 0 new_fls 0 (Array.length old_fls);
      new_fls

    let create () = Atomic.fetch_and_add counter 1

    exception Not_set

    let get_exn (type a) (Fiber r : fiber) (key : a t) =
      if key < Array.length r.fls && unique != Array.unsafe_get r.fls key then
        Sys.opaque_identity (Obj.magic (Array.unsafe_get r.fls key) : a)
      else raise_notrace Not_set

    let get (type a) (Fiber r : fiber) (key : a t) ~default =
      if key < Array.length r.fls && unique != Array.unsafe_get r.fls key then
        Sys.opaque_identity (Obj.magic (Array.unsafe_get r.fls key) : a)
      else default

    let set (type a) (Fiber r : fiber) (key : a t) (value : a) =
      let fls = r.fls in
      if key < Array.length fls then
        Array.unsafe_set fls key
          (Sys.opaque_identity (Obj.magic value : non_float))
      else
        let fls = grow fls key in
        r.fls <- fls;
        Array.unsafe_set fls key
          (Sys.opaque_identity (Obj.magic value : non_float))
  end
end

module Handler = struct
  type 'c t = {
    current : 'c -> Fiber.t;
    spawn : 'c -> Fiber.t -> (Fiber.t -> unit) -> unit;
    yield : 'c -> unit;
    cancel_after :
      'a. 'c -> 'a Computation.t -> seconds:float -> Exn_bt.t -> unit;
    await : 'c -> Trigger.t -> Exn_bt.t option;
  }
end
