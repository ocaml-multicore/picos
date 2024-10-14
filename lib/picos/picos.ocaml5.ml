module Trigger = struct
  (* BEGIN TRIGGER BOOTSTRAP *)

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
          match Atomic.get t with
          | Signaled -> false
          | any -> error_awaiting any
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

  (* END TRIGGER BOOTSTRAP *)

  type _ Effect.t += Await : t -> (exn * Printexc.raw_backtrace) option Effect.t

  let await t = if is_initial t then Effect.perform (Await t) else None
end

module Computation = struct
  (* BEGIN COMPUTATION BOOTSTRAP *)

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
  [@@warning "-37"]

  and ('a, _) st =
    | Canceled : {
        mutable tx : [ `Stopped | `Running ] tx;
        exn : exn;
        bt : Printexc.raw_backtrace;
      }
        -> ('a, [> `Canceled ]) st
    | Returned : {
        mutable tx : [ `Stopped | `Running ] tx;
        value : 'a;
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
    let[@inline never] impossible () = failwith "impossible"
    let[@inline never] already_committed () = invalid_arg "already committed"
    let[@inline] same (type a b) (x : a t) (y : b t) = x == (Obj.magic y : a t)

    type t = [ `Running ] tx

    let[@inline] create () =
      Running { state = Atomic.make Started; completions = Nil }

    let rec abort (Running r as tx : [ `Running ] tx) =
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
      in
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
          let success =
            Atomic.compare_and_set computation (S before) (S after)
          in
          if success then success
          else begin
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
      let success = Atomic.compare_and_set r.state Started Stopped in
      if not success then success else commit r.completions

    let[@inline] try_return (Running _ as tx : [ `Running ] tx) computation
        value =
      try_complete tx computation Backoff.default (Returned { value; tx })

    let[@inline] try_cancel (Running _ as tx : [ `Running ] tx) computation exn
        bt =
      try_complete tx computation Backoff.default (Canceled { exn; bt; tx })
  end

  let empty_fifo = S (Continue { triggers = []; balance_and_mode = fifo_bit })
  and empty_lifo = S (Continue { triggers = []; balance_and_mode = 0 })

  let create ?(mode : [ `FIFO | `LIFO ] = `FIFO) () =
    Atomic.make (if mode == `FIFO then empty_fifo else empty_lifo)

  let with_action ?(mode : [ `FIFO | `LIFO ] = `FIFO) x y action =
    let balance_and_mode = one + Bool.to_int (mode == `FIFO) in
    let trigger = Trigger.from_action x y action in
    Atomic.make (S (Continue { balance_and_mode; triggers = [ trigger ] }))

  let[@inline] is_canceled t =
    match Atomic.get t with
    | S (Canceled { tx; _ }) -> tx == Stopped
    | S (Returned _) | S (Continue _) -> false

  let[@inline never] canceled : (_, [ `Canceled | `Returned ]) st -> _ =
    function
    | Canceled { tx; exn; bt } -> if tx == Stopped then Some (exn, bt) else None
    | Returned _ -> None

  let[@inline] canceled t =
    match Atomic.get t with
    | S (Continue _) -> None
    | S ((Canceled _ | Returned _) as completed) -> canceled completed

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
        let success = Atomic.compare_and_set t before after in
        if success then success else try_attach t trigger (Backoff.once backoff)

  let try_attach t trigger = try_attach t trigger Backoff.default

  let rec unsafe_unsuspend t backoff =
    match Atomic.get t with
    | S (Continue r) as before ->
        let after =
          if fifo_bit <= r.balance_and_mode then
            let balance_and_mode = r.balance_and_mode - (2 * one) in
            S (Continue { r with balance_and_mode })
          else gc (r.balance_and_mode land fifo_bit) [] r.triggers
        in
        let success = Atomic.compare_and_set t before after in
        if success then success else unsafe_unsuspend t (Backoff.once backoff)
    | S ((Returned { tx; _ } | Canceled { tx; _ }) as was) ->
        if Tx.try_abort tx then unsafe_unsuspend t backoff
        else begin
          match (was : (_, [ `Returned | `Canceled ]) st) with
          | Returned _ -> true
          | Canceled _ -> false
        end

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

  let try_cancel t exn bt =
    try_terminate t (S (Canceled { exn; bt; tx = Stopped })) Backoff.default

  let return t value = try_return t value |> ignore
  let finish t = try_finish t |> ignore
  let cancel t exn bt = try_cancel t exn bt |> ignore

  let[@inline never] try_capture_raised exn t =
    try_cancel t exn (Printexc.get_raw_backtrace ())

  let try_capture t fn x =
    match fn x with
    | y -> try_return t y
    | exception exn -> try_capture_raised exn t

  let capture t fn x =
    (* Intentionally manually inlined [try_capture] to minimize stack usage *)
    (match fn x with
    | y -> try_return t y
    | exception exn -> try_capture_raised exn t)
    |> ignore

  let[@inline never] raise (Canceled { exn; bt; _ } : (_, [ `Canceled ]) st) =
    Printexc.raise_with_backtrace exn bt

  let check t =
    match Atomic.get t with
    | S (Canceled { tx; _ } as canceled) -> if tx == Stopped then raise canceled
    | S (Returned _) | S (Continue _) -> ()

  exception Running

  let peek_exn t =
    match Atomic.get t with
    | S ((Canceled { tx; _ } | Returned { tx; _ }) as was) ->
        if tx == Stopped then
          match (was : (_, [ `Returned | `Canceled ]) st) with
          | Canceled _ as canceled -> raise canceled
          | Returned { value; _ } -> value
        else raise_notrace Running
    | S (Continue _) -> raise_notrace Running

  let peek t =
    match Atomic.get t with
    | S (Canceled { exn; bt; tx; _ }) ->
        if tx == Stopped then Some (Error (exn, bt)) else None
    | S (Returned { value; tx; _ }) ->
        if tx == Stopped then Some (Ok value) else None
    | S (Continue _) -> None

  let propagate _ from into =
    match Atomic.get from with
    | S (Returned _) | S (Continue _) -> ()
    | S (Canceled _ as after) ->
        try_terminate into (S after) Backoff.default |> ignore

  let canceler ~from ~into = Trigger.from_action from into propagate

  let attach_canceler ~from ~into =
    let canceler = canceler ~from ~into in
    if try_attach from canceler then canceler else error_returned (check from)

  (* END COMPUTATION BOOTSTRAP *)

  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn : exn;
        bt : Printexc.raw_backtrace;
        computation : 'a t;
      }
        -> unit Effect.t

  let cancel_after computation ~seconds exn bt =
    if not (0.0 <= seconds) then error_negative_or_nan ()
    else Effect.perform (Cancel_after { seconds; exn; bt; computation })

  (* BEGIN COMPUTATION COMMON *)

  type ('a, _) op = Ignore : ('a, unit) op | Peek : ('a, 'a) op

  let block (type a r) (t : a t) (op : (a, r) op) : r =
    let trigger = Trigger.create () in
    if try_attach t trigger then begin
      match Trigger.await trigger with
      | None -> begin match op with Ignore -> () | Peek -> peek_exn t end
      | Some exn_bt ->
          unsafe_unsuspend t Backoff.default |> ignore;
          Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)
    end
    else begin
      match op with Ignore -> () | Peek -> peek_exn t
    end

  let await t =
    match Atomic.get t with
    | S (Continue _) -> block t Peek
    | S ((Returned { tx; _ } | Canceled { tx; _ }) as was) ->
        if tx == Stopped then
          match (was : (_, [ `Returned | `Canceled ]) st) with
          | Canceled _ as canceled -> raise canceled
          | Returned { value; _ } -> value
        else block t Peek

  let wait t = if is_running t then block t Ignore

  (* END COMPUTATION COMMON *)
end

module Fiber = struct
  (* BEGIN FIBER BOOTSTRAP *)

  let[@inline never] not_signaled () = invalid_arg "not signaled"

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

  let[@inline] is_canceled (Fiber r : t) =
    (not r.forbid)
    &&
    let (Packed computation) = r.packed in
    Computation.is_canceled computation

  let[@inline] canceled (Fiber r : t) =
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
          r.forbid <- not r.forbid;
          value
      | exception exn ->
          r.forbid <- not r.forbid;
          raise exn

  let forbid t body = explicitly t body ~forbid:true
  let permit t body = explicitly t body ~forbid:false

  let try_suspend (Fiber r : t) trigger x y resume =
    let (Packed computation) = r.packed in
    if not r.forbid then begin
      if Computation.try_attach computation trigger then
        let success = Trigger.on_signal trigger x y resume in
        if success then success
        else begin
          Computation.unsafe_unsuspend computation Backoff.default |> ignore;
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
    if Trigger.is_signaled trigger then
      r.forbid
      ||
      let (Packed computation) = r.packed in
      Computation.unsafe_unsuspend computation Backoff.default
    else not_signaled ()

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

    let reserve (type a) (Fiber r : fiber) (key : a t) =
      let fls = r.fls in
      if Array.length fls <= key then r.fls <- grow fls key

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

    let remove (type a) (Fiber r : fiber) (key : a t) =
      let fls = r.fls in
      if key < Array.length fls then Array.unsafe_set fls key unique
  end

  (* END FIBER BOOTSTRAP *)

  let resume t k = Effect.Deep.continue k (canceled t)
  let resume_with t k h = Effect.Shallow.continue_with k (canceled t) h

  let continue t k v =
    match canceled t with
    | None -> Effect.Deep.continue k v
    | Some (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt

  let continue_with t k v h =
    match canceled t with
    | None -> Effect.Shallow.continue_with k v h
    | Some (exn, bt) -> Effect.Shallow.discontinue_with_backtrace k exn bt h

  type _ Effect.t += Current : t Effect.t

  let current () = Effect.perform Current

  type _ Effect.t += Spawn : { fiber : t; main : t -> unit } -> unit Effect.t

  let spawn fiber main = Effect.perform @@ Spawn { fiber; main }

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield

  (* BEGIN FIBER COMMON *)

  module Maybe = struct
    let[@inline never] not_a_fiber _ = invalid_arg "not a fiber"

    type t = T : [< `Nothing | `Fiber ] tdt -> t [@@unboxed]

    let[@inline] to_fiber_or_current = function
      | T Nothing -> current ()
      | T (Fiber _ as t) -> t

    let[@inline] or_current t = T (to_fiber_or_current t)
    let nothing = T Nothing
    let[@inline] equal x y = x == y || x == nothing || y == nothing
    let[@inline] unequal x y = x != y || x == nothing
    let[@inline] of_fiber t = T t

    let[@inline] current_if checked =
      match checked with
      | None | Some true -> of_fiber (current ())
      | Some false -> nothing

    let[@inline] current_and_check_if checked =
      match checked with
      | None | Some true ->
          let fiber = current () in
          check fiber;
          of_fiber fiber
      | Some false -> nothing

    let[@inline] check = function
      | T Nothing -> ()
      | T (Fiber _ as t) -> check t

    let[@inline] to_fiber = function
      | T Nothing as any -> not_a_fiber any
      | T (Fiber _ as t) -> t
  end

  exception Done

  let empty_bt = Printexc.get_callstack 0

  let sleep ~seconds =
    let sleep = Computation.create ~mode:`LIFO () in
    Computation.cancel_after ~seconds sleep Done empty_bt;
    let trigger = Trigger.create () in
    if Computation.try_attach sleep trigger then
      match Trigger.await trigger with
      | None -> ()
      | Some exn_bt ->
          Computation.finish sleep;
          Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)

  (* END FIBER COMMON *)
end

module Handler = struct
  (* BEGIN HANDLER BOOTSTRAP *)

  type 'c t = {
    current : 'c -> Fiber.t;
    spawn : 'c -> Fiber.t -> (Fiber.t -> unit) -> unit;
    yield : 'c -> unit;
    cancel_after :
      'a.
      'c ->
      'a Computation.t ->
      seconds:float ->
      exn ->
      Printexc.raw_backtrace ->
      unit;
    await : 'c -> Trigger.t -> (exn * Printexc.raw_backtrace) option;
  }

  (* END HANDLER BOOTSTRAP *)

  let discontinue k exn =
    Effect.Deep.discontinue_with_backtrace k exn (Printexc.get_raw_backtrace ())

  let using (h : _ t) c =
    let current =
      Some
        (fun k ->
          match h.current c with
          | fiber -> Effect.Deep.continue k fiber
          | exception exn -> discontinue k exn)
    and yield =
      Some
        (fun k ->
          match h.yield c with
          | unit -> Effect.Deep.continue k unit
          | exception exn -> discontinue k exn)
    in
    let effc (type a) :
        a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
      | Fiber.Current -> current
      | Fiber.Spawn r ->
          Some
            (fun k ->
              match h.spawn c r.fiber r.main with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | Fiber.Yield -> yield
      | Trigger.Await trigger ->
          Some (fun k -> Effect.Deep.continue k (h.await c trigger))
      | Computation.Cancel_after r ->
          Some
            (fun k ->
              match
                h.cancel_after c r.computation ~seconds:r.seconds r.exn r.bt
              with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | _ -> None
    in
    let handler = Effect.Deep.{ retc = Fun.id; exnc = raise; effc } in
    fun main -> Effect.Deep.match_with main (h.current c) handler
end
