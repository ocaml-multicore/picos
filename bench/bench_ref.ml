open Multicore_bench
open Picos_std_sync

module Ref = struct
  type 'a t = 'a ref

  let make = ref

  let[@inline] compare_and_set x before after =
    !x == before
    && begin
         x := after;
         true
       end

  let[@inline] exchange x after =
    let before = !x in
    x := after;
    before
end

type t =
  | Op : string * 'a * ('a Ref.t -> _) * ('a Ref.t -> _) * [ `RW | `RO ] -> t

let run_one ~budgetf ?(n_iter = 250 * Util.iter_factor) ~lock_type
    (Op (name, value, op1, op2, op_kind)) =
  let lock = Lock.create () in
  let sem = Sem.create 1 in
  let rwlock = Rwlock.create () in

  let loc = Ref.make value in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    match (lock_type, op_kind) with
    | `Lock, _ ->
        let rec loop i =
          if i > 0 then begin
            Lock.acquire lock;
            op1 loc |> ignore;
            Lock.release lock;
            Lock.acquire lock;
            op2 loc |> ignore;
            Lock.release lock;
            loop (i - 2)
          end
        in
        loop n_iter
    | `Rwlock, `RW ->
        let rec loop i =
          if i > 0 then begin
            Rwlock.acquire rwlock;
            op1 loc |> ignore;
            Rwlock.release rwlock;
            Rwlock.acquire rwlock;
            op2 loc |> ignore;
            Rwlock.release rwlock;
            loop (i - 2)
          end
        in
        loop n_iter
    | `Rwlock, `RO ->
        let rec loop i =
          if i > 0 then begin
            Rwlock.acquire_shared rwlock;
            op1 loc |> ignore;
            Rwlock.release_shared rwlock;
            Rwlock.acquire_shared rwlock;
            op2 loc |> ignore;
            Rwlock.release_shared rwlock;
            loop (i - 2)
          end
        in
        loop n_iter
    | `Sem, _ ->
        let rec loop i =
          if i > 0 then begin
            Sem.acquire sem;
            op1 loc |> ignore;
            Sem.release sem;
            Sem.acquire sem;
            op2 loc |> ignore;
            Sem.release sem;
            loop (i - 2)
          end
        in
        loop n_iter
  in

  let config =
    Printf.sprintf "%s with %s" name
      (match lock_type with
      | `Lock -> "Lock"
      | `Rwlock -> "Rwlock"
      | `Sem -> "Sem")
  in
  Times.record ~budgetf ~n_domains:1 ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_iter ~singular:"op" ~config

let run_suite ~budgetf =
  Util.cross [ `Lock; `Rwlock; `Sem ]
    [
      (let get x = !x in
       Op ("get", 42, get, get, `RO));
      (let incr x = x := !x + 1 in
       Op ("incr", 0, incr, incr, `RW));
      (let push x = x := 101 :: !x
       and pop x = match !x with [] -> () | _ :: xs -> x := xs in
       Op ("push & pop", [], push, pop, `RW));
      (let cas01 x = Ref.compare_and_set x 0 1
       and cas10 x = Ref.compare_and_set x 1 0 in
       Op ("cas int", 0, cas01, cas10, `RW));
      (let xchg1 x = Ref.exchange x 1 and xchg0 x = Ref.exchange x 0 in
       Op ("xchg int", 0, xchg1, xchg0, `RW));
      (let swap x =
         let l, r = !x in
         x := (r, l)
       in
       Op ("swap", (4, 2), swap, swap, `RW));
    ]
  |> List.concat_map @@ fun (lock_type, op) -> run_one ~budgetf ~lock_type op
