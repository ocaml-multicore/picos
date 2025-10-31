open Multicore_bench
open Picos_std_sync

module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

module Hashtbl = Hashtbl.Make (Key)

let run_one ~budgetf ~n_domains ?(n_ops = 100 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) ~lock_type () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Hashtbl.create 1000 in
  let lock = Lock.create ~padded:true () in
  let rwlock = Rwlock.create ~padded:true () in
  let sem = Sem.create ~padded:true 1 in

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Countdown.create ~n_domains () in

  let before () =
    begin match lock_type with
    | `Lock ->
        Lock.holding lock @@ fun () ->
        Hashtbl.clear t;
        if prepopulate then begin
          for _ = 1 to n_keys do
            let value = Random.bits () in
            let key = value mod n_keys in
            Hashtbl.replace t key value
          done
        end
    | `Rwlock ->
        Rwlock.holding rwlock @@ fun () ->
        Hashtbl.clear t;
        if prepopulate then begin
          for _ = 1 to n_keys do
            let value = Random.bits () in
            let key = value mod n_keys in
            Hashtbl.replace t key value
          done
        end
    | `Sem ->
        Sem.acquire sem;
        Hashtbl.clear t;
        if prepopulate then begin
          for _ = 1 to n_keys do
            let value = Random.bits () in
            let key = value mod n_keys in
            Hashtbl.replace t key value
          done
        end;
        Sem.release sem
    end;
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let init _ = Scheduler.run @@ fun () -> Random.State.make_self_init () in
  let wrap _ _ = Scheduler.run in
  let work domain_index state =
    match lock_type with
    | `Lock ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              let value = Random.State.bits state in
              let op = (value asr 20) mod 100 in
              let key = value mod n_keys in
              if op < percent_mem then begin
                Lock.acquire lock;
                Hashtbl.find_opt t key |> ignore;
                Lock.release lock
              end
              else if op < limit_add then begin
                Lock.acquire lock;
                Hashtbl.replace t key value;
                Lock.release lock
              end
              else begin
                Lock.acquire lock;
                Hashtbl.remove t key;
                Lock.release lock
              end
            done;
            work ()
          end
        in
        work ()
    | `Rwlock ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              let value = Random.State.bits state in
              let op = (value asr 20) mod 100 in
              let key = value mod n_keys in
              if op < percent_mem then begin
                Rwlock.acquire_shared rwlock;
                Hashtbl.find_opt t key |> ignore;
                Rwlock.release_shared rwlock
              end
              else if op < limit_add then begin
                Rwlock.acquire rwlock;
                Hashtbl.replace t key value;
                Rwlock.release rwlock
              end
              else begin
                Rwlock.acquire rwlock;
                Hashtbl.remove t key;
                Rwlock.release rwlock
              end
            done;
            work ()
          end
        in
        work ()
    | `Sem ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              let value = Random.State.bits state in
              let op = (value asr 20) mod 100 in
              let key = value mod n_keys in
              if op < percent_mem then begin
                Sem.acquire sem;
                Hashtbl.find_opt t key |> ignore;
                Sem.release sem
              end
              else if op < limit_add then begin
                Sem.acquire sem;
                Hashtbl.replace t key value;
                Sem.release sem
              end
              else begin
                Sem.acquire sem;
                Hashtbl.remove t key;
                Sem.release sem
              end
            done;
            work ()
          end
        in
        work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads with %s" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
      (match lock_type with
      | `Lock -> "Lock"
      | `Rwlock -> "Rwlock"
      | `Sem -> "Sem")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~before ~init
    ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4; 8 ]
    (Util.cross [ `Lock; `Rwlock; `Sem ] [ 10; 50; 90; 95; 100 ])
  |> List.concat_map @@ fun (n_domains, (lock_type, percent_mem)) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~percent_mem ~lock_type ()
