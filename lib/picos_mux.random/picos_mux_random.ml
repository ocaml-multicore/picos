open Picos
module Htbl = Picos_aux_htbl

module Collection = struct
  type 'a t = (int, 'a) Htbl.t

  module Key = struct
    type t = int

    let equal = Int.equal
    let hash = Fun.id
  end

  let create () = Htbl.create ~hashed_type:(module Key) ()

  let rec push t value =
    let key = Random.bits () in
    if not (Htbl.try_add t key value) then push t value

  let rec pop_exn t =
    let key = Htbl.find_random_exn t in
    try Htbl.remove_exn t key with Not_found -> pop_exn t

  let is_empty t =
    match Htbl.find_random_exn t with _ -> false | exception Not_found -> true
end

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Current of Fiber.t * (Fiber.t, unit) Effect.Deep.continuation
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of
      Fiber.t
      * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
  | Raise of
      Fiber.t
      * (unit, unit) Effect.Deep.continuation
      * exn
      * Printexc.raw_backtrace
  | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

type t = {
  ready : ready Collection.t;
  mutable num_waiters_non_zero : bool;
  num_alive_fibers : int Atomic.t;
  mutable resume :
    Trigger.t ->
    Fiber.t ->
    ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
    unit;
  num_waiters : int ref;
  condition : Condition.t;
  mutex : Mutex.t;
  mutable current : ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
  mutable yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
  mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
  mutable handler : (unit, unit) Effect.Deep.handler;
  mutable run : bool;
}

let[@inline] relaxed_wakeup t ~known_not_empty =
  if
    t.num_waiters_non_zero
    && (known_not_empty || not (Collection.is_empty t.ready))
  then begin
    Mutex.lock t.mutex;
    Mutex.unlock t.mutex;
    Condition.signal t.condition
  end

let exec (p : Fiber.Per_thread.t) t ready =
  p.current <-
    (match ready with
    | Spawn (fiber, _)
    | Raise (fiber, _, _, _)
    | Return (fiber, _)
    | Current (fiber, _)
    | Continue (fiber, _)
    | Resume (fiber, _) ->
        Fiber.Maybe.of_fiber fiber);
  match ready with
  | Spawn (fiber, main) -> Effect.Deep.match_with main fiber t.handler
  | Raise (_, k, exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt
  | Return (_, k) -> Effect.Deep.continue k ()
  | Current (fiber, k) -> Effect.Deep.continue k fiber
  | Continue (fiber, k) -> Fiber.continue fiber k ()
  | Resume (fiber, k) -> Fiber.resume fiber k

let rec next p t =
  match Collection.pop_exn t.ready with
  | ready ->
      relaxed_wakeup t ~known_not_empty:false;
      exec p t ready
  | exception Not_found ->
      p.current <- Fiber.Maybe.nothing;
      if Atomic.get t.num_alive_fibers <> 0 then begin
        Mutex.lock t.mutex;
        let n = !(t.num_waiters) + 1 in
        t.num_waiters := n;
        if n = 1 then t.num_waiters_non_zero <- true;
        if Collection.is_empty t.ready && Atomic.get t.num_alive_fibers <> 0
        then begin
          match Condition.wait t.condition t.mutex with
          | () ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero <- false;
              Mutex.unlock t.mutex;
              next p t
          | exception async_exn ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero <- false;
              Mutex.unlock t.mutex;
              raise async_exn
        end
        else begin
          let n = !(t.num_waiters) - 1 in
          t.num_waiters := n;
          if n = 0 then t.num_waiters_non_zero <- false;
          Mutex.unlock t.mutex;
          next p t
        end
      end
      else begin
        Mutex.lock t.mutex;
        Mutex.unlock t.mutex;
        Condition.broadcast t.condition
      end

let default_fatal_exn_handler exn =
  prerr_string "Fatal error: exception ";
  prerr_string (Printexc.to_string exn);
  prerr_char '\n';
  Printexc.print_backtrace stderr;
  flush stderr;
  exit 2

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
  let t =
    {
      ready = Collection.create ();
      num_waiters_non_zero = false;
      num_alive_fibers = Atomic.make 1 |> Multicore_magic.copy_as_padded;
      resume = Obj.magic ();
      num_waiters = ref 0 |> Multicore_magic.copy_as_padded;
      condition = Condition.create ();
      mutex = Mutex.create ();
      current = Obj.magic ();
      yield = Obj.magic ();
      return = Obj.magic ();
      handler = Obj.magic ();
      run = false;
    }
  in
  t.resume <-
    (fun trigger fiber k ->
      let resume = Resume (fiber, k) in
      Fiber.unsuspend fiber trigger |> ignore;
      Collection.push t.ready resume;
      let non_zero =
        match Mutex.lock t.mutex with
        | () ->
            let non_zero = t.num_waiters_non_zero in
            Mutex.unlock t.mutex;
            non_zero
        | exception Sys_error _ -> false
      in
      if non_zero then Condition.signal t.condition);
  t.current <-
    Some
      (fun k ->
        let p = Fiber.Per_thread.get () in
        let fiber = Fiber.Maybe.to_fiber p.current in
        Collection.push t.ready (Current (fiber, k));
        next p t);
  t.yield <-
    Some
      (fun k ->
        let p = Fiber.Per_thread.get () in
        let fiber = Fiber.Maybe.to_fiber p.current in
        Collection.push t.ready (Continue (fiber, k));
        next p t);
  t.return <-
    Some
      (fun k ->
        let p = Fiber.Per_thread.get () in
        let fiber = Fiber.Maybe.to_fiber p.current in
        Collection.push t.ready (Return (fiber, k));
        next p t);
  t.handler <-
    {
      retc =
        (fun () ->
          Atomic.decr t.num_alive_fibers;
          let p = Fiber.Per_thread.get () in
          next p t);
      exnc;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Fiber.Current ->
              (t.current : ((a, _) Effect.Deep.continuation -> _) option)
          | Fiber.Spawn r ->
              let p = Fiber.Per_thread.get () in
              let fiber = Fiber.Maybe.to_fiber p.current in
              if Fiber.is_canceled fiber then t.yield
              else begin
                Atomic.incr t.num_alive_fibers;
                Collection.push t.ready (Spawn (r.fiber, r.main));
                relaxed_wakeup t ~known_not_empty:true;
                t.return
              end
          | Fiber.Yield -> t.yield
          | Computation.Cancel_after r -> begin
              let p = Fiber.Per_thread.get () in
              let fiber = Fiber.Maybe.to_fiber p.current in
              if Fiber.is_canceled fiber then t.yield
              else
                match
                  Select.cancel_after r.computation ~seconds:r.seconds r.exn
                    r.bt
                with
                | () -> t.return
                | exception exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    Some
                      (fun k ->
                        Collection.push t.ready (Raise (fiber, k, exn, bt));
                        next p t)
            end
          | Trigger.Await trigger ->
              Some
                (fun k ->
                  let p = Fiber.Per_thread.get () in
                  let fiber = Fiber.Maybe.to_fiber p.current in
                  if Fiber.try_suspend fiber trigger fiber k t.resume then
                    next p t
                  else begin
                    Collection.push t.ready (Resume (fiber, k));
                    next p t
                  end)
          | _ -> None);
    };
  t

let runner_on_this_thread t =
  Select.check_configured ();
  next (Fiber.Per_thread.get ()) t

let rec await t =
  if t.num_waiters_non_zero then begin
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
    let p = Fiber.Per_thread.get () in
    p.current <- Fiber.Maybe.of_fiber fiber;
    Effect.Deep.match_with main fiber t.handler;
    Mutex.lock t.mutex;
    await t
  end

let[@inline never] run ?context fiber main computation =
  run_fiber ?context fiber main;
  Computation.peek_exn computation

let run ?context ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run ?context fiber main computation

let rec run_fiber_on n fiber main runner_main context =
  if n <= 1 then run_fiber ~context fiber main
  else
    let runner =
      try Domain.spawn runner_main
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        run ~context Fun.id;
        Printexc.raise_with_backtrace exn bt
    in
    match run_fiber_on (n - 1) fiber main runner_main context with
    | result ->
        begin
          match Domain.join runner with
          | None -> ()
          | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
        end;
        result
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        begin
          match Domain.join runner with
          | None -> ()
          | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
        end;
        Printexc.raise_with_backtrace exn bt

let run_fiber_on ?fatal_exn_handler ~n_domains fiber main =
  if n_domains < 1 then invalid_arg "n_domains must be positive";
  let context = context ?fatal_exn_handler () in
  let runner_main =
    if n_domains = 1 then fun () -> None
    else
      let bt_status = Printexc.backtrace_status () in
      fun () ->
        Printexc.record_backtrace bt_status;
        match runner_on_this_thread context with
        | () -> None
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Some (exn, bt)
  in
  run_fiber_on n_domains fiber main runner_main context

let[@inline never] run_on ?fatal_exn_handler ~n_domains fiber main computation =
  run_fiber_on ?fatal_exn_handler ~n_domains fiber main;
  Computation.peek_exn computation

let run_on ?fatal_exn_handler ~n_domains ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_on ?fatal_exn_handler ~n_domains fiber main computation
