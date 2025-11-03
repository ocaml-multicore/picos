open Picos
open Picos_std_awaitable

type t = int Awaitable.t

let create ?padded () = Awaitable.make ?padded 0

let[@inline] wait t lock ~acquire ~release =
  let before = Awaitable.get t in
  release lock;
  let lock_forbidden lock ~acquire =
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
  let await t lock ~acquire before =
    match if Awaitable.get t == before then Awaitable.await t before with
    | () ->
        let before = Awaitable.get t in
        if before land 1 = 0 then begin
          let after = before lor 1 in
          if
            (not (Awaitable.compare_and_set t before after))
            && Awaitable.get t land 1 = 0
          then Awaitable.signal t
        end;
        lock_forbidden lock ~acquire
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        lock_forbidden lock ~acquire;
        Printexc.raise_with_backtrace exn bt
  in
  if before land 1 = 0 then
    let after = before lor 1 in
    if Awaitable.compare_and_set t before after || Awaitable.get t == after then
      await t lock ~acquire after
    else lock_forbidden lock ~acquire
  else await t lock ~acquire before

let signal t =
  let prior = Awaitable.fetch_and_add t 2 in
  if prior land 1 <> 0 then begin
    if Awaitable.compare_and_set t (prior + 2) (prior + 1) then ();
    Awaitable.signal t
  end

let broadcast t =
  let prior = Awaitable.fetch_and_add t 2 in
  if prior land 1 <> 0 then begin
    if Awaitable.compare_and_set t (prior + 2) (prior + 1) then ();
    Awaitable.broadcast t
  end
