open Picos_bootstrap

let error () =
  raise (Sys_error "Picos.set_picos_implementation must be called exactly once")

type implementation = {
  current : unit -> Fiber.t;
  spawn : 'a. forbid:bool -> 'a Computation.t -> (unit -> unit) list -> unit;
  yield : unit -> unit;
  cancel_after : 'a. 'a Computation.t -> seconds:float -> Exn_bt.t -> unit;
  await : Trigger.t -> Exn_bt.t option;
}

let implementation =
  let current = error
  and spawn ~forbid:_ _ _ = error ()
  and yield = error
  and cancel_after _ ~seconds:_ _ = error ()
  and await _ = error () in
  ref { current; spawn; yield; cancel_after; await }

module Trigger = struct
  let await t = if Trigger.is_initial t then !implementation.await t else None
end

module Fiber = struct
  let current () = !implementation.current ()

  let spawn ~forbid computation mains =
    let _ = current () in
    !implementation.spawn ~forbid computation mains

  let yield () =
    let _ = current () in
    !implementation.yield ()
end

module Computation = struct
  let cancel_after computation ~seconds exn_bt =
    Computation.check_non_negative seconds;
    let _ = Fiber.current () in
    !implementation.cancel_after computation ~seconds exn_bt
end

let[@poll error] [@inline never] set_picos_implementation i =
  !implementation.current == error
  && begin
       implementation := i;
       true
     end

let set_picos_implementation (module I : Implementation) =
  let i =
    {
      current = I.Fiber.current;
      spawn = I.Fiber.spawn;
      yield = I.Fiber.yield;
      cancel_after = I.Computation.cancel_after;
      await = I.Trigger.await;
    }
  in
  if not (set_picos_implementation i) then error ()
