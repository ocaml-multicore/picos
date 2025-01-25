open Picos
open Picos_std_awaitable

type t = unit Awaitable.t

let create ?padded () = Awaitable.make ?padded ()

let[@inline] wait t lock ~acquire ~release =
  let trigger = Trigger.create () in
  let awaiter = Awaitable.Awaiter.add t trigger in
  release lock;
  let lock_forbidden lock =
    let fiber = Fiber.current () in
    let forbid = Fiber.exchange fiber ~forbid:true in
    match acquire lock with
    | () -> Fiber.set fiber ~forbid
    | exception exn ->
        (* The lock was poisoned or frozen. *)
        let bt = Printexc.get_raw_backtrace () in
        Fiber.set fiber ~forbid;
        Printexc.raise_with_backtrace exn bt
  in
  match Trigger.await trigger with
  | None -> lock_forbidden lock
  | Some exn_bt ->
      Awaitable.Awaiter.remove awaiter;
      lock_forbidden lock;
      Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)

let signal = Awaitable.signal
let broadcast = Awaitable.broadcast
