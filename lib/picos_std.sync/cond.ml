open Picos
open Picos_std_awaitable

type t = unit Awaitable.t

let create ?padded () = Awaitable.make ?padded ()

let[@inline] wait t mutex ~lock ~unlock =
  let trigger = Trigger.create () in
  let awaiter = Awaitable.Awaiter.add t trigger in
  unlock mutex;
  let lock_forbidden mutex =
    let fiber = Fiber.current () in
    let forbid = Fiber.exchange fiber ~forbid:true in
    lock mutex;
    Fiber.set fiber ~forbid
  in
  match Trigger.await trigger with
  | None -> lock_forbidden mutex
  | Some exn_bt ->
      Awaitable.Awaiter.remove awaiter;
      lock_forbidden mutex;
      Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)

let signal = Awaitable.signal
let broadcast = Awaitable.broadcast
