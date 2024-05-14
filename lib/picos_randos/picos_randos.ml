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
  | Spawn of Fiber.t * (unit -> unit)
  | Current of Fiber.t * (Fiber.t, unit) Effect.Deep.continuation
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Deep.continuation
  | Raise of (unit, unit) Effect.Deep.continuation * Exn_bt.t
  | Return of (unit, unit) Effect.Deep.continuation

type t = {
  ready : ready Collection.t;
  num_waiters_non_zero : bool ref;
  num_alive_fibers : int Atomic.t;
  resume :
    Trigger.t ->
    Fiber.t ->
    (Exn_bt.t option, unit) Effect.Deep.continuation ->
    unit;
  retc : unit -> unit;
  num_waiters : int ref;
  condition : Condition.t;
  mutex : Mutex.t;
  mutable run : bool;
}

let rec spawn t n forbid packed = function
  | [] -> Atomic.fetch_and_add t.num_alive_fibers n |> ignore
  | main :: mains ->
      let fiber = Fiber.create_packed ~forbid packed in
      Collection.push t.ready (Spawn (fiber, main));
      if !(t.num_waiters_non_zero) then Condition.signal t.condition;
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
        Mutex.lock t.mutex;
        if Collection.is_empty t.ready && Atomic.get t.num_alive_fibers <> 0
        then begin
          let n = !(t.num_waiters) + 1 in
          t.num_waiters := n;
          if n = 1 then t.num_waiters_non_zero := true;
          match Condition.wait t.condition t.mutex with
          | () ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero := false;
              Mutex.unlock t.mutex;
              next t
          | exception async_exn ->
              let n = !(t.num_waiters) - 1 in
              t.num_waiters := n;
              if n = 0 then t.num_waiters_non_zero := false;
              Mutex.unlock t.mutex;
              raise async_exn
        end
        else begin
          Mutex.unlock t.mutex;
          next t
        end
      end
      else begin
        Mutex.lock t.mutex;
        Mutex.unlock t.mutex;
        Condition.broadcast t.condition
      end

let context () =
  Select.check_configured ();
  let rec t =
    {
      ready = Collection.create ();
      num_waiters_non_zero = ref false |> Multicore_magic.copy_as_padded;
      num_alive_fibers = Atomic.make 1 |> Multicore_magic.copy_as_padded;
      resume;
      retc;
      num_waiters = ref 0 |> Multicore_magic.copy_as_padded;
      condition = Condition.create ();
      mutex = Mutex.create ();
      run = false;
    }
  and retc () =
    Atomic.decr t.num_alive_fibers;
    next t
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

let runner_on_this_thread = next

let rec await t computation =
  if !(t.num_waiters_non_zero) then begin
    match Condition.wait t.condition t.mutex with
    | () -> await t computation
    | exception async_exn ->
        Mutex.unlock t.mutex;
        raise async_exn
  end
  else begin
    Mutex.unlock t.mutex;
    Computation.await computation
  end

let run ?context:t_opt ?(forbid = false) main =
  let t = match t_opt with Some t -> t | None -> context () in
  Mutex.lock t.mutex;
  if t.run then begin
    Mutex.unlock t.mutex;
    invalid_arg "already run"
  end
  else begin
    t.run <- true;
    Mutex.unlock t.mutex;
    let computation = Computation.create () in
    let fiber = Fiber.create ~forbid computation in
    let main = Computation.capture computation main in
    Collection.push t.ready (Spawn (fiber, main));
    next t;
    Mutex.lock t.mutex;
    await t computation
  end
