open Picos
open Picos_std_awaitable

type t = int Awaitable.t

let create ?padded () = Awaitable.make ?padded 0

let[@inline] wait t lock ~acquire ~release =
  let before = Awaitable.get t in
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
  match if Awaitable.get t == before then Awaitable.await t before with
  | () -> lock_forbidden lock
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      lock_forbidden lock;
      Printexc.raise_with_backtrace exn bt

let signal t =
  Awaitable.incr t;
  Awaitable.signal t

let broadcast t =
  Awaitable.incr t;
  Awaitable.broadcast t
