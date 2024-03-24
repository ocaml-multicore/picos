open Picos
module Queue = Picos_mpsc_queue

(* As a minor optimization, we avoid allocating closures, which take slightly
   more memory than values of this type. *)
type ready =
  | Spawn of Fiber.t * (unit -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Deep.continuation

type t = {
  ready : ready Queue.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int Atomic.t;
  mc : Picos_ptmc.t;
      (** We must store this for [resume], which may come from any thread. *)
  resume :
    Trigger.t ->
    Fiber.t ->
    (Exn_bt.t option, unit) Effect.Deep.continuation ->
    unit;
  retc : unit -> unit;
}

let rec spawn t n forbid computation = function
  | [] -> Atomic.fetch_and_add t.num_alive_fibers n |> ignore
  | main :: mains ->
      let fiber = Fiber.create ~forbid computation in
      Queue.push t.ready (Spawn (fiber, main));
      spawn t (n + 1) forbid computation mains

let continue = Some (fun k -> Effect.Deep.continue k ())

let rec next t =
  match Queue.pop_exn t.ready with
  | Spawn (fiber, main) ->
      let current =
        (* The current handler must never propagate cancelation, but it would be
           possible to continue some other fiber and resume the current fiber
           later. *)
        Some (fun k -> Effect.Deep.continue k fiber)
      and yield =
        Some
          (fun k ->
            Queue.push t.ready (Continue (fiber, k));
            next t)
      and discontinue = Some (fun k -> Fiber.continue fiber k ()) in
      let[@alert "-handler"] effc (type a) :
          a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
        | Fiber.Current ->
            (* We handle [Current] first as it is perhaps the most latency
               sensitive effect. *)
            current
        | Fiber.Spawn r ->
            (* We check cancelation status once and then either perform the
               whole operation or discontinue the fiber. *)
            if Fiber.is_canceled fiber then discontinue
            else begin
              spawn t 0 r.forbid r.computation r.mains;
              continue
            end
        | Fiber.Yield -> yield
        | Computation.Cancel_after r ->
            (* We check cancelation status once and then either perform the
               whole operation or discontinue the fiber. *)
            if Fiber.is_canceled fiber then discontinue
            else begin
              Picos_select.cancel_after r.computation ~seconds:r.seconds
                r.exn_bt;
              continue
            end
        | Trigger.Await trigger ->
            Some
              (fun k ->
                if Fiber.try_suspend fiber trigger fiber k t.resume then next t
                else Fiber.resume fiber k)
        | _ -> None
      in
      Effect.Deep.match_with main () { retc = t.retc; exnc = raise; effc }
  | Continue (fiber, k) -> Fiber.continue fiber k ()
  | Resume (fiber, k) -> Fiber.resume fiber k
  | exception Queue.Empty ->
      if Atomic.get t.num_alive_fibers <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Picos_ptmc.lock t.mc;
          match if Atomic.get t.needs_wakeup then Picos_ptmc.wait t.mc with
          | () -> Picos_ptmc.unlock t.mc
          | exception exn ->
              Picos_ptmc.unlock t.mc;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run ~forbid main =
  let ready = Queue.create ()
  and needs_wakeup = Atomic.make false
  and num_alive_fibers = Atomic.make 1
  and mc = Picos_ptmc.get () in
  let rec t = { ready; needs_wakeup; num_alive_fibers; mc; resume; retc }
  and retc () =
    Atomic.decr t.num_alive_fibers;
    next t
  and resume trigger fiber k =
    let resume = Resume (fiber, k) in
    if Fiber.unsuspend fiber trigger then
      (* The fiber has not been canceled, so we queue the fiber normally. *)
      Queue.push t.ready resume
    else
      (* The fiber has been canceled, so we give priority to it in this
         scheduler. *)
      Queue.push_head t.ready resume;
    (* As the trigger might have been signaled from another domain or systhread
       outside of the scheduler, we check whether the scheduler needs to be
       woken up and take care of it if necessary. *)
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then Picos_ptmc.broadcast t.mc
  in
  let computation = Computation.create () in
  let fiber = Fiber.create ~forbid computation in
  let main = Computation.capture computation main in
  Queue.push t.ready (Spawn (fiber, main));
  next t;
  Computation.await computation
