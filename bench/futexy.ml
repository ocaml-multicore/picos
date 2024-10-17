open Picos
open Picos_std_sync

module Mutex = struct
  type t = int Awaitable.t

  let create ?padded () = Awaitable.make ?padded 0

  let rec lock t old =
    if old <> 0 then begin
      Awaitable.await t 2;
      lock t (Awaitable.exchange t 2)
    end

  let lock ?checked:_ t =
    if not (Awaitable.compare_and_set t 0 1) then
      lock t (Awaitable.exchange t 2)

  let unlock ?checked:_ t =
    let before = Awaitable.fetch_and_add t (-1) in
    if before = 2 then begin
      Awaitable.set t 0;
      Awaitable.signal t
    end
end

module Condition = struct
  type t = Trigger.t list Atomic.t

  let create ?padded () = Atomic.make [] |> Multicore_magic.copy_as ?padded

  let rec add t trigger backoff =
    let before = Atomic.get t in
    let after = trigger :: before in
    if not (Atomic.compare_and_set t before after) then
      add t trigger (Backoff.once backoff)

  let wait t mutex =
    let trigger = Trigger.create () in
    add t trigger Backoff.default;
    Mutex.unlock mutex;
    let result = Trigger.await trigger in
    let fiber = Fiber.current () in
    let forbid = Fiber.exchange fiber ~forbid:true in
    Mutex.lock mutex;
    Fiber.set fiber ~forbid;
    match result with
    | None -> ()
    | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

  let broadcast t =
    if Atomic.get t != [] then
      Atomic.exchange t [] |> List.rev |> List.iter Trigger.signal
end
