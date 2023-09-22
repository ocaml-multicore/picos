open Picos

type t = {
  ready : (unit -> unit) Mpsc_queue.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int ref;
  mc : Mutex_and_condition.t;
}

let rec next t =
  match Mpsc_queue.dequeue t.ready with
  | work -> work ()
  | exception Mpsc_queue.Empty ->
      if !(t.num_alive_fibers) <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Mutex_and_condition.lock t.mc;
          match Mutex_and_condition.wait t.mc with
          | () -> Mutex_and_condition.unlock t.mc
          | exception exn ->
              Mutex_and_condition.unlock t.mc;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run ~forbid main =
  let ready = Mpsc_queue.create () in
  let needs_wakeup = Atomic.make false in
  let num_alive_fibers = ref 1 in
  let mc = Mutex_and_condition.get () in
  let t = { ready; needs_wakeup; num_alive_fibers; mc } in
  let resume trigger fiber k =
    if not (Fiber.has_forbidden fiber) then Fiber.detach fiber trigger;
    let work () = Effect.Deep.continue k (Fiber.canceled fiber) in
    Mpsc_queue.enqueue t.ready work;
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then Mutex_and_condition.broadcast t.mc
  in
  let rec fork fiber main =
    let current =
      Some
        (fun k ->
          match Fiber.canceled fiber with
          | None -> Effect.Deep.continue k fiber
          | Some exn_bt -> Exn_bt.discontinue k exn_bt)
    and yield =
      Some
        (fun k ->
          Mpsc_queue.enqueue t.ready (fun () ->
              match Fiber.canceled fiber with
              | None -> Effect.Deep.continue k ()
              | Some exn_bt -> Exn_bt.discontinue k exn_bt);
          next t)
    in
    let effc (type a) :
        a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
      | Fiber.Current ->
          (* We handle [Current] first as it is perhaps the most latency
             sensitive effect. *)
          current
      | Fiber.Spawn { forbid; computation; mains } ->
          Some
            (fun k ->
              match Fiber.canceled fiber with
              | None ->
                  mains
                  |> List.iter (fun main ->
                         let fiber = Fiber.create ~forbid computation in
                         Mpsc_queue.enqueue t.ready (fun () -> fork fiber main);
                         incr t.num_alive_fibers);
                  Effect.Deep.continue k ()
              | Some exn_bt -> Exn_bt.discontinue k exn_bt)
      | Fiber.Yield -> yield
      | Trigger.Await trigger ->
          (* We handle [Await] last as it is probably the least latency
             sensitive effect. *)
          Some
            (fun k ->
              if Fiber.has_forbidden fiber then
                if Trigger.on_signal trigger fiber k resume then next t
                else Effect.Deep.continue k None
              else if Fiber.try_attach fiber trigger then
                if Trigger.on_signal trigger fiber k resume then next t
                else begin
                  Fiber.detach fiber trigger;
                  Effect.Deep.continue k (Fiber.canceled fiber)
                end
              else begin
                Trigger.signal trigger;
                Effect.Deep.continue k (Fiber.canceled fiber)
              end)
      | _ -> None
    and retc () =
      decr t.num_alive_fibers;
      next t
    in
    Effect.Deep.match_with main () { retc; exnc = raise; effc }
  in
  let computation = Computation.create () in
  fork (Fiber.create ~forbid computation) (Computation.capture computation main);
  Computation.await computation
