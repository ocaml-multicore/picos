open Picos

type t = { fiber : Fiber.t; mutex : Mutex.t; condition : Condition.t }

let create_packed ~forbid packed =
  let fiber = Fiber.create_packed ~forbid packed in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  { fiber; mutex; condition }

let rec block trigger t =
  if not (Trigger.is_signaled trigger) then begin
    (* We block fibers (or threads) on a per thread mutex and condition. *)
    Mutex.lock t.mutex;
    match
      if not (Trigger.is_signaled trigger) then
        (* We assume that there is no poll point after the above [Mutex.lock]
           and before the below [Condition.wait] is ready to be woken up by a
           [Condition.broadcast]. *)
        Condition.wait t.condition t.mutex
    with
    | () ->
        Mutex.unlock t.mutex;
        block trigger t
    | exception async_exn ->
        (* Condition.wait may be interrupted by asynchronous exceptions and we
           must make sure to unlock even in that case. *)
        Mutex.unlock t.mutex;
        raise async_exn
  end

let resume trigger t _ =
  let _is_canceled : bool = Fiber.unsuspend t.fiber trigger in
  (* This will be called when the trigger is signaled.  We simply broadcast on
     the per thread condition variable. *)
  begin
    match Mutex.lock t.mutex with
    | () -> Mutex.unlock t.mutex
    | exception Sys_error _ ->
        (* This should mean that [resume] was called from a signal handler
           running on the scheduler thread.  If the assumption about not having
           poll points holds, the [Condition.broadcast] should now be able to
           wake up the [Condition.wait] in the scheduler. *)
        ()
  end;
  Condition.broadcast t.condition

let[@alert "-handler"] rec await t trigger =
  if Fiber.try_suspend t.fiber trigger t t resume then block trigger t;
  Fiber.canceled t.fiber

and current t =
  (* The current handler must never propagate cancelation, but it would be
     possible to yield here to run some other fiber before resuming the current
     fiber. *)
  t.fiber

and yield t =
  (* In other handlers we need to account for cancelation. *)
  Fiber.check t.fiber;
  Thread.yield ()

and cancel_after : type a. _ -> a Computation.t -> _ =
 (* We need an explicit type signature to allow OCaml to generalize the tyoe as
    all of the handlers are in a single recursive definition. *)
 fun t computation ~seconds exn_bt ->
  Fiber.check t.fiber;
  Select.cancel_after computation ~seconds exn_bt

and spawn : type a. _ -> forbid:bool -> a Computation.t -> _ =
 fun t ~forbid computation mains ->
  Fiber.check t.fiber;
  let packed = Computation.Packed computation in
  match mains with
  | [ main ] ->
      Thread.create
        (fun () ->
          (* We need to (recursively) install the handler on each new thread
             that we create. *)
          Handler.using handler (create_packed ~forbid packed) main)
        ()
      |> ignore
  | mains -> begin
      (* We try to be careful to implement the all-or-nothing behaviour based on
         the assumption that we may run out of threads well before we run out of
         memory.  In a thread pool based scheduler this should actually not
         require special treatment. *)
      let all_or_nothing = ref `Wait in
      match
        mains
        |> List.iter @@ fun main ->
           Thread.create
             (fun () ->
               if !all_or_nothing == `Wait then begin
                 Mutex.lock t.mutex;
                 match
                   while
                     match !all_or_nothing with
                     | `Wait ->
                         Condition.wait t.condition t.mutex;
                         true
                     | `All | `Nothing -> false
                   do
                     ()
                   done
                 with
                 | () -> Mutex.unlock t.mutex
                 | exception async_exn ->
                     (* Condition.wait may be interrupted by asynchronous
                        exceptions and we must make sure to unlock even in that
                        case. *)
                     Mutex.unlock t.mutex;
                     raise async_exn
               end;
               if !all_or_nothing == `All then
                 (* We need to (recursively) install the handler on each new
                    thread that we create. *)
                 Handler.using handler (create_packed ~forbid packed) main)
             ()
           |> ignore
      with
      | () ->
          Mutex.lock t.mutex;
          all_or_nothing := `All;
          Mutex.unlock t.mutex;
          Condition.broadcast t.condition
      | exception exn ->
          Mutex.lock t.mutex;
          all_or_nothing := `Nothing;
          Mutex.unlock t.mutex;
          Condition.broadcast t.condition;
          raise exn
    end

and handler = Handler.{ current; spawn; yield; cancel_after; await }

let run ?(forbid = false) main =
  Select.check_configured ();
  let computation = Computation.create ~mode:`LIFO () in
  let context = create_packed ~forbid (Packed computation) in
  let main () =
    Computation.capture computation main ();
    Computation.await computation
  in
  Handler.using handler context main
