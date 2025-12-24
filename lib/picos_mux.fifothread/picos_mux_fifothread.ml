open Picos

let[@inline never] quota_non_positive _ = invalid_arg "quota must be positive"
(*let[@inline never] not_worker _ = invalid_arg "not a worker thread"*)

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of
      Fiber.t
      * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
  | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

module Mpmcq = Picos_aux_mpmcq

(* Must have per_thread state for [fiber], for example! *)

type t = {
  ready : ready Mpmcq.t;
  needs_wakeup : bool Atomic.t;
  mutex : Mutex.t;
  condition : Condition.t;
  mutable resume :
    Trigger.t ->
    Fiber.t ->
    ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
    unit;
  mutable counter : int;
  quota : int;
  mutable num_alive_fibers : int;
  mutable owner : int;
  main : int;
  mutable fatal_exn : (exn * Printexc.raw_backtrace) option;
  fatal_exn_handler : exn -> unit;
  computation : unit Computation.t;
}

type _ tdt =
  | Per_thread : {
      mutable fiber : Fiber.Maybe.t;
      mutable remaining_quota : int;
      context : t;
      id : int;
      mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable discontinue :
        ((unit, unit) Effect.Deep.continuation -> unit) option;
    }
      -> [> `Per_thread ] tdt

type per_thread = [ `Per_thread ] tdt

let per_thread_key = Picos_thread.TLS.create ()

let[@inline] get_per_thread () : per_thread =
  Picos_thread.TLS.get_exn per_thread_key

let default_fatal_exn_handler t exn =
  let bt = Printexc.get_raw_backtrace () in
  t.fatal_exn <- Some (exn, bt)

let exnc exn =
  let (Per_thread p) = get_per_thread () in
  let t = p.context in
  default_fatal_exn_handler t exn;
  t.fatal_exn_handler exn

exception Finished

let rec retc () =
  let (Per_thread p as pt) = get_per_thread () in
  let t = p.context in
  t.num_alive_fibers <- t.num_alive_fibers - 1;
  next pt

and effc : type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
  function
  | Fiber.Current -> current
  | Fiber.Spawn r ->
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else
        let t = p.context in
        t.num_alive_fibers <- t.num_alive_fibers + 1;
        Mpmcq.push t.ready (Spawn (r.fiber, r.main));
        p.return
  | Fiber.Yield -> yield
  | Computation.Cancel_after r -> begin
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else
        match
          Picos_io_select.cancel_after r.computation ~seconds:r.seconds r.exn
            r.bt
        with
        | () -> p.return
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Some (fun k -> Effect.Deep.discontinue_with_backtrace k exn bt)
    end
  | Trigger.Await trigger ->
      Some
        (fun k ->
          let (Per_thread p as pt) = get_per_thread () in
          let fiber = Fiber.Maybe.to_fiber p.fiber in
          if Fiber.try_suspend fiber trigger fiber k p.context.resume then
            next pt
          else
            let remaining_quota = p.remaining_quota - 1 in
            if 0 < remaining_quota then begin
              p.remaining_quota <- remaining_quota;
              Fiber.resume fiber k
            end
            else begin
              Mpmcq.push p.context.ready (Resume (fiber, k));
              next pt
            end)
  | _ -> None

and handler = { Effect.Deep.exnc; effc; retc }

and next (Per_thread p as pt : per_thread) =
  let t = p.context in
  if t.owner == p.id then begin
    match Mpmcq.pop_exn t.ready with
    | ready -> begin
        t.counter <- t.counter + 1;
        p.remaining_quota <- t.quota;
        let fiber =
          match ready with
          | Spawn (fiber, _)
          | Continue (fiber, _)
          | Resume (fiber, _)
          | Return (fiber, _) ->
              fiber
        in
        p.fiber <- Fiber.Maybe.of_fiber fiber;
        match ready with
        | Spawn (_, main) -> Effect.Deep.match_with main fiber handler
        | Continue (_, k) -> Fiber.continue fiber k ()
        | Resume (_, k) -> Fiber.resume fiber k
        | Return (_, k) -> Effect.Deep.continue k ()
      end
    | exception Mpmcq.Empty ->
        p.fiber <- Fiber.Maybe.nothing;
        if t.num_alive_fibers <> 0 && t.fatal_exn == None then begin
          if Atomic.get t.needs_wakeup then begin
            Mutex.lock t.mutex;
            match
              if Atomic.get t.needs_wakeup then
                Condition.wait t.condition t.mutex
            with
            | () -> Mutex.unlock t.mutex
            | exception exn ->
                Mutex.unlock t.mutex;
                raise exn
          end
          else Atomic.set t.needs_wakeup true;
          next pt
        end
        else
          Computation.cancel t.computation Finished (Printexc.get_callstack 0)
  end

and yield : ((unit, _) Effect.Deep.continuation -> _) option =
  Some
    (fun k ->
      let (Per_thread p as pt) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      Mpmcq.push p.context.ready (Continue (fiber, k));
      next pt)

and current : ((Fiber.t, _) Effect.Deep.continuation -> _) option =
  Some
    (fun k ->
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      Effect.Deep.continue k fiber)

let check t =
  match t.fatal_exn with
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let worker t =
  Picos_io_select.check_configured ();
  let id = Thread.id (Thread.self ()) in
  let (Per_thread p as pt : per_thread) =
    Per_thread
      {
        fiber = Fiber.Maybe.nothing;
        remaining_quota = 0;
        context = t;
        id;
        return = None;
        discontinue = None;
      }
    |> Multicore_magic.copy_as_padded
  in
  p.return <-
    Some
      (fun k ->
        let (Per_thread p : per_thread) = pt in
        let t = p.context in
        (* Check owner first? *)
        let remaining_quota = p.remaining_quota - 1 in
        if 0 < remaining_quota then begin
          p.remaining_quota <- remaining_quota;
          Effect.Deep.continue k ()
        end
        else begin
          Mpmcq.push t.ready (Return (Fiber.Maybe.to_fiber p.fiber, k));
          next pt
        end);
  p.discontinue <-
    Some
      (fun k ->
        let (Per_thread p : per_thread) = pt in
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Fiber.continue fiber k ());
  Picos_thread.TLS.set per_thread_key pt;
  if t.owner = t.main then begin
    t.counter <- t.counter + 1;
    t.owner <- p.id;
    next pt
  end

let watcher t =
  let fiber = Fiber.create ~forbid:false t.computation in
  Picos_mux_thread.run_fiber fiber begin fun _ ->
      try
        let seen_counter = ref t.counter in
        while t.num_alive_fibers > 0 && t.fatal_exn == None do
          if
            t.owner <> t.main
            && (not (Mpmcq.is_empty t.ready))
            && t.counter == !seen_counter
          then begin
            t.owner <- t.main;
            let _ = Thread.create worker t in
            ()
          end;
          seen_counter := t.counter;
          Picos.Fiber.sleep ~seconds:0.1
        done
      with
      | Finished -> ()
      | exn -> exnc exn
    end;
  Mutex.lock t.mutex;
  Mutex.unlock t.mutex;
  Condition.broadcast t.condition;
  check t

let run_fiber ?quota ?(fatal_exn_handler = ignore) fiber main =
  Picos_io_select.check_configured ();
  let t =
    let quota =
      match quota with
      | None -> Int.max_int
      | Some quota -> if quota <= 0 then quota_non_positive quota else quota
    in
    let main = Thread.id (Thread.self ()) in
    {
      ready = Mpmcq.create ~padded:true ();
      needs_wakeup = Atomic.make false |> Multicore_magic.copy_as_padded;
      mutex = Mutex.create ();
      condition = Condition.create ();
      resume = Obj.magic ();
      counter = 0;
      quota;
      num_alive_fibers = 1;
      owner = main + 1;
      main;
      fatal_exn = None;
      fatal_exn_handler;
      computation = Computation.create ();
    }
    |> Multicore_magic.copy_as_padded
  in
  t.resume <-
    (fun trigger fiber k ->
      let resume = Resume (fiber, k) in
      if Fiber.unsuspend fiber trigger then Mpmcq.push t.ready resume
      else Mpmcq.push_head t.ready resume;
      if
        Atomic.get t.needs_wakeup
        && Atomic.compare_and_set t.needs_wakeup true false
      then begin
        begin match Mutex.lock t.mutex with
        | () -> Mutex.unlock t.mutex
        | exception Sys_error _ -> ()
        end;
        Condition.broadcast t.condition
      end);
  Mpmcq.push t.ready (Spawn (fiber, main));
  watcher t

let[@inline never] run ?quota ?fatal_exn_handler fiber main computation =
  run_fiber ?quota ?fatal_exn_handler fiber main;
  Computation.peek_exn computation

let run ?quota ?fatal_exn_handler ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run ?quota ?fatal_exn_handler fiber main computation
