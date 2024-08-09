open Picos

module Collection = struct
  type 'a t = (int, 'a) Picos_htbl.t

  module Key = struct
    type t = int

    let equal = Int.equal
    let hash = Fun.id
  end

  let create () = Picos_htbl.create ~hashed_type:(module Key) ()

  let rec push t value =
    let key = Random.bits () in
    if not (Picos_htbl.try_add t key value) then push t value

  let rec pop_exn t =
    let key = Picos_htbl.find_random_exn t in
    try Picos_htbl.remove_exn t key with Not_found -> pop_exn t

  let is_empty t =
    match Picos_htbl.find_random_exn t with
    | _ -> false
    | exception Not_found -> true
end

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Current of Fiber.t * (Fiber.t, unit) Effect.Shallow.continuation
  | Continue of Fiber.t * (unit, unit) Effect.Shallow.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Shallow.continuation
  | Raise of Fiber.t * (unit, unit) Effect.Shallow.continuation * Exn_bt.t
  | Return of Fiber.t * (unit, unit) Effect.Shallow.continuation

type t = {
  ready : ready Collection.t;
  num_waiters_non_zero : bool ref;
  num_alive_fibers : int Atomic.t;
  resume :
    Trigger.t ->
    Fiber.t ->
    (Exn_bt.t option, unit) Effect.Shallow.continuation ->
    unit;
  exnc : exn -> unit;
  num_waiters : int ref;
  condition : Condition.t;
  mutex : Mutex.t;
  mutable run : bool;
}

type per_thread = {
  context : t;
  current : ((Fiber.t, unit) Effect.Shallow.continuation -> unit) option;
  yield : ((unit, unit) Effect.Shallow.continuation -> unit) option;
  return : ((unit, unit) Effect.Shallow.continuation -> unit) option;
  handler : (unit, unit) Effect.Shallow.handler;
  mutable fiber : Fiber.Maybe.t;
}

let rec next p =
  match Collection.pop_exn p.context.ready with
  | Spawn (fiber, main) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      let k = Effect.Shallow.fiber main in
      Effect.Shallow.continue_with k fiber p.handler
  | Raise (fiber, k, exn_bt) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Exn_bt.discontinue_with k exn_bt p.handler
  | Return (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Effect.Shallow.continue_with k () p.handler
  | Current (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Effect.Shallow.continue_with k fiber p.handler
  | Continue (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Fiber.continue_with fiber k () p.handler
  | Resume (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Fiber.resume_with fiber k p.handler
  | exception Not_found ->
      p.fiber <- Fiber.Maybe.nothing;
      if Atomic.get p.context.num_alive_fibers <> 0 then begin
        Mutex.lock p.context.mutex;
        if
          Collection.is_empty p.context.ready
          && Atomic.get p.context.num_alive_fibers <> 0
        then begin
          let n = !(p.context.num_waiters) + 1 in
          p.context.num_waiters := n;
          if n = 1 then p.context.num_waiters_non_zero := true;
          match Condition.wait p.context.condition p.context.mutex with
          | () ->
              let n = !(p.context.num_waiters) - 1 in
              p.context.num_waiters := n;
              if n = 0 then p.context.num_waiters_non_zero := false;
              Mutex.unlock p.context.mutex;
              next p
          | exception async_exn ->
              let n = !(p.context.num_waiters) - 1 in
              p.context.num_waiters := n;
              if n = 0 then p.context.num_waiters_non_zero := false;
              Mutex.unlock p.context.mutex;
              raise async_exn
        end
        else begin
          Mutex.unlock p.context.mutex;
          next p
        end
      end
      else begin
        Mutex.lock p.context.mutex;
        Mutex.unlock p.context.mutex;
        Condition.broadcast p.context.condition
      end

let default_fatal_exn_handler exn =
  prerr_string "Fatal error: exception ";
  prerr_string (Printexc.to_string exn);
  prerr_char '\n';
  Printexc.print_backtrace stderr;
  flush stderr;
  exit 2

let per_thread context =
  let rec p =
    { context; current; yield; return; handler; fiber = Fiber.Maybe.nothing }
  and current =
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Collection.push p.context.ready (Current (fiber, k));
        next p)
  and yield =
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Collection.push p.context.ready (Continue (fiber, k));
        next p)
  and return =
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Collection.push p.context.ready (Return (fiber, k));
        next p)
  and handler = { retc; exnc = context.exnc; effc }
  and[@alert "-handler"] effc :
      type a. a Effect.t -> ((a, _) Effect.Shallow.continuation -> _) option =
    function
    | Fiber.Current -> p.current
    | Fiber.Spawn r ->
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        if Fiber.is_canceled fiber then p.yield
        else begin
          Atomic.incr p.context.num_alive_fibers;
          Collection.push p.context.ready (Spawn (r.fiber, r.main));
          if !(p.context.num_waiters_non_zero) then
            Condition.signal p.context.condition;
          p.return
        end
    | Fiber.Yield -> p.yield
    | Computation.Cancel_after r -> begin
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        if Fiber.is_canceled fiber then p.yield
        else
          match
            Select.cancel_after r.computation ~seconds:r.seconds r.exn_bt
          with
          | () -> p.return
          | exception exn ->
              let exn_bt = Exn_bt.get exn in
              Some
                (fun k ->
                  Collection.push p.context.ready (Raise (fiber, k, exn_bt));
                  next p)
      end
    | Trigger.Await trigger ->
        Some
          (fun k ->
            let fiber = Fiber.Maybe.to_fiber p.fiber in
            if Fiber.try_suspend fiber trigger fiber k p.context.resume then
              next p
            else begin
              Collection.push p.context.ready (Resume (fiber, k));
              next p
            end)
    | _ -> None
  and retc () =
    Atomic.decr p.context.num_alive_fibers;
    next p
  in
  p

let context ?fatal_exn_handler () =
  Select.check_configured ();
  let exnc =
    match fatal_exn_handler with
    | None -> default_fatal_exn_handler
    | Some handler ->
        fun exn ->
          handler exn;
          raise exn
  in
  let rec t =
    {
      ready = Collection.create ();
      num_waiters_non_zero = ref false |> Multicore_magic.copy_as_padded;
      num_alive_fibers = Atomic.make 1 |> Multicore_magic.copy_as_padded;
      resume;
      exnc;
      num_waiters = ref 0 |> Multicore_magic.copy_as_padded;
      condition = Condition.create ();
      mutex = Mutex.create ();
      run = false;
    }
  and resume trigger fiber k =
    let resume = Resume (fiber, k) in
    Fiber.unsuspend fiber trigger |> ignore;
    Collection.push t.ready resume;
    let non_zero =
      match Mutex.lock t.mutex with
      | () ->
          let non_zero = !(t.num_waiters_non_zero) in
          Mutex.unlock t.mutex;
          non_zero
      | exception Sys_error _ -> false
    in
    if non_zero then Condition.signal t.condition
  in
  t

let runner_on_this_thread t =
  Select.check_configured ();
  next (per_thread t)

let rec await t =
  if !(t.num_waiters_non_zero) then begin
    match Condition.wait t.condition t.mutex with
    | () -> await t
    | exception async_exn ->
        Mutex.unlock t.mutex;
        raise async_exn
  end
  else Mutex.unlock t.mutex

let run_fiber ?context:t_opt fiber main =
  let t =
    match t_opt with
    | Some t ->
        Select.check_configured ();
        t
    | None -> context ()
  in
  Mutex.lock t.mutex;
  if t.run then begin
    Mutex.unlock t.mutex;
    invalid_arg "already run"
  end
  else begin
    t.run <- true;
    Mutex.unlock t.mutex;
    Collection.push t.ready (Spawn (fiber, main));
    next (per_thread t);
    Mutex.lock t.mutex;
    await t
  end

let run ?context ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?context fiber main;
  Computation.await computation

let rec run_fiber_on n fiber main context =
  if n <= 1 then run_fiber ~context fiber main
  else
    let runner =
      try Domain.spawn @@ fun () -> runner_on_this_thread context
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        run ~context Fun.id;
        Printexc.raise_with_backtrace exn bt
    in
    match run_fiber_on (n - 1) fiber main context with
    | result ->
        Domain.join runner;
        result
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Domain.join runner;
        Printexc.raise_with_backtrace exn bt

let run_fiber_on ?fatal_exn_handler ~n_domains fiber main =
  if n_domains < 1 then invalid_arg "n_domains must be positive";
  run_fiber_on n_domains fiber main (context ?fatal_exn_handler ())

let run_on ?fatal_exn_handler ~n_domains ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber_on ?fatal_exn_handler ~n_domains fiber main;
  Computation.await computation
