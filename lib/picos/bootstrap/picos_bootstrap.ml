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

  type 'a state =
    | Canceled of Exn_bt.t
    | Returned of 'a
    | Continue of { balance_and_mode : int; triggers : Trigger.t list }

  type 'a t = 'a state Atomic.t

  let fifo_bit = 1
  let one = 2

  let empty_fifo = Continue { triggers = []; balance_and_mode = fifo_bit }
  and empty_lifo = Continue { triggers = []; balance_and_mode = 0 }

  let create ?(mode : [ `FIFO | `LIFO ] = `FIFO) () =
    Atomic.make (if mode == `FIFO then empty_fifo else empty_lifo)

  let with_action ?(mode : [ `FIFO | `LIFO ] = `FIFO) x y action =
    let balance_and_mode = one + Bool.to_int (mode == `FIFO) in
    let trigger = Trigger.from_action x y action in
    Atomic.make (Continue { balance_and_mode; triggers = [ trigger ] })

  let is_canceled t =
    match Atomic.get t with
    | Canceled _ -> true
    | Returned _ | Continue _ -> false

  let canceled t =
    match Atomic.get t with
    | Canceled exn_bt -> Some exn_bt
    | Returned _ | Continue _ -> None

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
          Continue { balance_and_mode; triggers }
    | r :: rs ->
        if Trigger.is_signaled r then gc balance_and_mode triggers rs
        else gc (balance_and_mode + one) (r :: triggers) rs

  let rec try_attach t trigger backoff =
    match Atomic.get t with
    | Returned _ | Canceled _ -> false
    | Continue r as before ->
        (* We check the trigger before potential allocations. *)
        (not (Trigger.is_signaled trigger))
        &&
        let after =
          if fifo_bit <= r.balance_and_mode then
            let balance_and_mode = r.balance_and_mode + one in
            let triggers = trigger :: r.triggers in
            Continue { balance_and_mode; triggers }
          else
            gc (one + (r.balance_and_mode land fifo_bit)) [ trigger ] r.triggers
        in
        Atomic.compare_and_set t before after
        || try_attach t trigger (Backoff.once backoff)

  let try_attach t trigger = try_attach t trigger Backoff.default

  let rec unsafe_unsuspend t backoff =
    match Atomic.get t with
    | Returned _ -> true
    | Canceled _ -> false
    | Continue r as before ->
        let after =
          if fifo_bit <= r.balance_and_mode then
            Continue
              { r with balance_and_mode = r.balance_and_mode - (2 * one) }
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
    | Canceled _ | Returned _ -> false
    | Continue _ -> true

  let rec try_terminate t after backoff =
    match Atomic.get t with
    | Returned _ | Canceled _ -> false
    | Continue r as before ->
        if Atomic.compare_and_set t before after then begin
          List.iter Trigger.signal
            (if r.balance_and_mode land fifo_bit = fifo_bit then
               List.rev r.triggers
             else r.triggers);
          true
        end
        else try_terminate t after (Backoff.once backoff)

  let returned_unit = Returned (Obj.magic ())

  let make_returned value =
    if value == Obj.magic () then returned_unit else Returned value

  let returned value = Atomic.make (make_returned value)
  let finished = Atomic.make (make_returned ())
  let try_return t value = try_terminate t (make_returned value) Backoff.default
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

  let propagate _ from into =
    match Atomic.get from with
    | Returned _ | Continue _ -> ()
    | Canceled _ as after -> try_terminate into after Backoff.default |> ignore

  let canceler ~from ~into = Trigger.from_action from into propagate

  let check_non_negative seconds =
    if not (0.0 <= seconds) then error_negative_or_nan ()

  let rec get_or block t =
    match Atomic.get t with
    | Returned value -> value
    | Canceled exn_bt -> Exn_bt.raise exn_bt
    | Continue _ -> get_or block (block t)

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
    type 'a key = { index : int; default : non_float; compute : unit -> 'a }

    let compute () = failwith "impossible"
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

    type 'a initial = Constant of 'a | Computed of (unit -> 'a)

    let new_key initial =
      let index = Atomic.fetch_and_add counter 1 in
      match initial with
      | Constant default ->
          let default = Sys.opaque_identity (Obj.magic default : non_float) in
          { index; default; compute }
      | Computed compute -> { index; default = unique; compute }

    let get (type a) (Fiber r : t) (key : a key) =
      let fls = r.fls in
      if key.index < Array.length fls then begin
        let value = Array.unsafe_get fls key.index in
        if value != unique then Sys.opaque_identity (Obj.magic value : a)
        else
          let value = key.default in
          if value != unique then begin
            (* As the [fls] array was already large enough, we cache the default
               value in the array. *)
            Array.unsafe_set fls key.index value;
            Sys.opaque_identity (Obj.magic value : a)
          end
          else
            let value = key.compute () in
            Array.unsafe_set fls key.index
              (Sys.opaque_identity (Obj.magic value : non_float));
            value
      end
      else
        let value = key.default in
        if value != unique then Sys.opaque_identity (Obj.magic value : a)
        else
          let value = key.compute () in
          let fls = grow fls key.index in
          r.fls <- fls;
          Array.unsafe_set fls key.index
            (Sys.opaque_identity (Obj.magic value : non_float));
          value

    let set (type a) (Fiber r : t) (key : a key) (value : a) =
      let fls = r.fls in
      if key.index < Array.length fls then
        Array.unsafe_set fls key.index
          (Sys.opaque_identity (Obj.magic value : non_float))
      else
        let fls = grow fls key.index in
        r.fls <- fls;
        Array.unsafe_set fls key.index
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
