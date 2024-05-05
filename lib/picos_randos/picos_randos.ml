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

  let pop_exn t =
    let key = Picos_htbl.find_random_exn t in
    Picos_htbl.remove_exn t key
end

type ready =
  | Spawn of Fiber.t * (unit -> unit)
  | Current of Fiber.t * (Fiber.t, unit) Effect.Deep.continuation
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Deep.continuation
  | Raise of (unit, unit) Effect.Deep.continuation * Exn_bt.t
  | Return of (unit, unit) Effect.Deep.continuation

type t = {
  ready : ready Collection.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int Atomic.t;
  mutex : Mutex.t;
  condition : Condition.t;
  resume :
    Trigger.t ->
    Fiber.t ->
    (Exn_bt.t option, unit) Effect.Deep.continuation ->
    unit;
  retc : unit -> unit;
}

let rec spawn t n forbid packed = function
  | [] -> Atomic.fetch_and_add t.num_alive_fibers n |> ignore
  | main :: mains ->
      let fiber = Fiber.create_packed ~forbid packed in
      Collection.push t.ready (Spawn (fiber, main));
      spawn t (n + 1) forbid packed mains

let rec next t =
  match Collection.pop_exn t.ready with
  | Spawn (fiber, main) ->
      let current =
        Some
          (fun k ->
            Collection.push t.ready (Current (fiber, k));
            next t)
      and yield =
        Some
          (fun k ->
            Collection.push t.ready (Continue (fiber, k));
            next t)
      and return =
        Some
          (fun k ->
            Collection.push t.ready (Return k);
            next t)
      in
      let[@alert "-handler"] effc (type a) :
          a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
        | Fiber.Current -> current
        | Fiber.Spawn r ->
            if Fiber.is_canceled fiber then yield
            else begin
              spawn t 0 r.forbid (Packed r.computation) r.mains;
              return
            end
        | Fiber.Yield -> yield
        | Computation.Cancel_after r -> begin
            if Fiber.is_canceled fiber then yield
            else
              match
                Select.cancel_after r.computation ~seconds:r.seconds r.exn_bt
              with
              | () -> return
              | exception exn ->
                  let exn_bt = Exn_bt.get exn in
                  Some
                    (fun k ->
                      Collection.push t.ready (Raise (k, exn_bt));
                      next t)
          end
        | Trigger.Await trigger ->
            Some
              (fun k ->
                if Fiber.try_suspend fiber trigger fiber k t.resume then next t
                else begin
                  Collection.push t.ready (Resume (fiber, k));
                  next t
                end)
        | _ -> None
      in
      Effect.Deep.match_with main () { retc = t.retc; exnc = raise; effc }
  | Raise (k, exn_bt) -> Exn_bt.discontinue k exn_bt
  | Return k -> Effect.Deep.continue k ()
  | Current (fiber, k) -> Effect.Deep.continue k fiber
  | Continue (fiber, k) -> Fiber.continue fiber k ()
  | Resume (fiber, k) -> Fiber.resume fiber k
  | exception Not_found ->
      if Atomic.get t.num_alive_fibers <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Mutex.lock t.mutex;
          match
            if Atomic.get t.needs_wakeup then Condition.wait t.condition t.mutex
          with
          | () -> Mutex.unlock t.mutex
          | exception exn ->
              Mutex.unlock t.mutex;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run ?(forbid = false) main =
  Select.check_configured ();
  let ready = Collection.create ()
  and needs_wakeup = Atomic.make false
  and num_alive_fibers = Atomic.make 1
  and mutex = Mutex.create ()
  and condition = Condition.create () in
  let rec t =
    { ready; needs_wakeup; num_alive_fibers; mutex; condition; resume; retc }
  and retc () =
    Atomic.decr t.num_alive_fibers;
    next t
  and resume trigger fiber k =
    let resume = Resume (fiber, k) in
    Fiber.unsuspend fiber trigger |> ignore;
    Collection.push t.ready resume;
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then begin
      begin
        match Mutex.lock t.mutex with
        | () -> Mutex.unlock t.mutex
        | exception Sys_error _ -> ()
      end;
      Condition.broadcast t.condition
    end
  in
  let computation = Computation.create () in
  let fiber = Fiber.create ~forbid computation in
  let main = Computation.capture computation main in
  Collection.push t.ready (Spawn (fiber, main));
  next t;
  Computation.await computation
