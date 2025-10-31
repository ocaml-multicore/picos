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

  let incr = incr

  let[@inline] swap x =
    let l, r = !x in
    x := (r, l)

  let[@inline] push x = x := 101 :: !x
  let[@inline] pop x = match !x with [] -> () | _ :: xs -> x := xs
end

type _ op =
  | Get : int op
  | Incr : int op
  | Push_and_pop : int list op
  | Cas_int : int op
  | Xchg_int : int op
  | Swap : (int * int) op

let run_one (type a) ~budgetf ?(n_iter = 250 * Util.iter_factor) ~lock_type
    (op : a op) =
  let lock = Lock.create () in
  let sem = Sem.create 1 in
  let rwlock = Rwlock.create () in

  let name, (value : a) =
    match op with
    | Get -> ("get", 42)
    | Incr -> ("incr", 0)
    | Push_and_pop -> ("push & pop", [])
    | Cas_int -> ("cas int", 0)
    | Xchg_int -> ("xchg int", 0)
    | Swap -> ("swap", (4, 2))
  in

  let loc = Ref.make value in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    match (lock_type, op) with
    | `Lock, _ -> begin
        let acquire = Lock.acquire and release = Lock.release and lock = lock in
        match op with
        | Get ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                let a = !(Sys.opaque_identity loc) in
                release lock;
                acquire lock;
                let b = !(Sys.opaque_identity loc) in
                release lock;
                loop (i - 2 + (a - b))
              end
            in
            loop n_iter
        | Incr ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.incr loc;
                release lock;
                acquire lock;
                Ref.incr loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Push_and_pop ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.push loc;
                release lock;
                acquire lock;
                Ref.pop loc |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Cas_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.compare_and_set loc 0 1 |> ignore;
                release lock;
                acquire lock;
                Ref.compare_and_set loc 1 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Xchg_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.exchange loc 1 |> ignore;
                release lock;
                acquire lock;
                Ref.exchange loc 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Swap ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.swap loc;
                release lock;
                acquire lock;
                Ref.swap loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
      end
    | `Rwlock, Get -> begin
        let acquire = Rwlock.acquire_shared
        and release = Rwlock.release_shared
        and lock = rwlock in
        match op with
        | Get ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                let a = !(Sys.opaque_identity loc) in
                release lock;
                acquire lock;
                let b = !(Sys.opaque_identity loc) in
                release lock;
                loop (i - 2 + (a - b))
              end
            in
            loop n_iter
        | _ -> ()
      end
    | `Rwlock, _ -> begin
        let acquire = Rwlock.acquire
        and release = Rwlock.release
        and lock = rwlock in
        match op with
        | Get -> ()
        | Incr ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.incr loc;
                release lock;
                acquire lock;
                Ref.incr loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Push_and_pop ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.push loc;
                release lock;
                acquire lock;
                Ref.pop loc |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Cas_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.compare_and_set loc 0 1 |> ignore;
                release lock;
                acquire lock;
                Ref.compare_and_set loc 1 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Xchg_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.exchange loc 1 |> ignore;
                release lock;
                acquire lock;
                Ref.exchange loc 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Swap ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.swap loc;
                release lock;
                acquire lock;
                Ref.swap loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
      end
    | `Sem, _ -> begin
        let acquire = Sem.acquire and release = Sem.release and lock = sem in
        match op with
        | Get ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                let a = !(Sys.opaque_identity loc) in
                release lock;
                acquire lock;
                let b = !(Sys.opaque_identity loc) in
                release lock;
                loop (i - 2 + (a - b))
              end
            in
            loop n_iter
        | Incr ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.incr loc;
                release lock;
                acquire lock;
                Ref.incr loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Push_and_pop ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.push loc;
                release lock;
                acquire lock;
                Ref.pop loc |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Cas_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.compare_and_set loc 0 1 |> ignore;
                release lock;
                acquire lock;
                Ref.compare_and_set loc 1 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Xchg_int ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.exchange loc 1 |> ignore;
                release lock;
                acquire lock;
                Ref.exchange loc 0 |> ignore;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
        | Swap ->
            let rec loop i =
              if i > 0 then begin
                acquire lock;
                Ref.swap loc;
                release lock;
                acquire lock;
                Ref.swap loc;
                release lock;
                loop (i - 2)
              end
            in
            loop n_iter
      end
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
  [ `Lock; `Rwlock; `Sem ]
  |> List.concat_map @@ fun lock_type ->
     [
       run_one ~budgetf ~lock_type Get;
       run_one ~budgetf ~lock_type Incr;
       run_one ~budgetf ~lock_type Push_and_pop;
       run_one ~budgetf ~lock_type Cas_int;
       run_one ~budgetf ~lock_type Xchg_int;
       run_one ~budgetf ~lock_type Swap;
     ]
     |> List.concat
