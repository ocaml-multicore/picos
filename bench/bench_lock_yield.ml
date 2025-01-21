open Multicore_bench
open Picos_std_structured
open Picos_std_sync

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

(** This will keep a domain running. *)
let yielder () =
  while true do
    Control.yield ()
  done

let run_one ~budgetf ~n_fibers ~use_domains ~lock_type () =
  let n_domains = if use_domains then n_fibers else 1 in
  let n_ops =
    (if use_domains || is_ocaml4 then 10 else 100) * Util.iter_factor
  in

  let v = ref 0 |> Multicore_magic.copy_as_padded in
  let n_ops_todo = Countdown.create ~n_domains () in

  let lock = Lock.create ~padded:true () in
  let rwlock = Rwlock.create ~padded:true () in
  let sem =
    Sem.create ~padded:true (match lock_type with `Sem_n n -> n | _ -> 1)
  in
  let counter = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let batch = if use_domains || n_fibers < 16 then 1000 else 100 in

  let init _ =
    assert (!v = 0);
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    Flock.join_after @@ fun () ->
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch in
      match lock_type with
      | `Lock ->
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Lock.acquire lock;
                let x = !v in
                v := x + 1;
                Control.yield ();
                assert (!v = x + 1);
                v := x;
                Lock.release lock;
                loop (n - 1)
              end
              else work ()
            in
            loop n
      | `Rwlock ->
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Rwlock.acquire rwlock;
                let x = !v in
                v := x + 1;
                Control.yield ();
                assert (!v = x + 1);
                v := x;
                Rwlock.release rwlock;
                loop (n - 1)
              end
              else work ()
            in
            loop n
      | `Sem ->
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Sem.acquire sem;
                let x = !v in
                v := x + 1;
                Control.yield ();
                assert (!v = x + 1);
                v := x;
                Sem.release sem;
                loop (n - 1)
              end
              else work ()
            in
            loop n
      | `Sem_n n_resources ->
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Sem.acquire sem;
                Atomic.incr counter;
                Control.yield ();
                let n_live = Atomic.fetch_and_add counter (-1) in
                assert (n_live <= n_resources);
                Sem.release sem;
                loop (n - 1)
              end
              else work ()
            in
            loop n
    in
    if use_domains then begin
      if not is_ocaml4 then Flock.fork yielder;
      work ();
      Flock.terminate ()
    end
    else
      for _ = 1 to n_fibers do
        Flock.fork work
      done
  in

  let config =
    Printf.sprintf "%d %s%s with %s" n_fibers
      (if use_domains then "domain" else "fiber")
      (if n_fibers = 1 then "" else "s")
      (match lock_type with
      | `Lock -> "Lock"
      | `Rwlock -> "Rwlock"
      | `Sem -> "Sem"
      | `Sem_n n -> Printf.sprintf "Sem %d" n)
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"locked yield" ~config

let run_suite ~budgetf =
  Util.cross [ false; true ]
    (Util.cross
       [ `Lock; `Rwlock; `Sem; `Sem_n 2; `Sem_n 3; `Sem_n 4 ]
       [ 1; 2; 3; 4; 8; 256; 512; 1024 ])
  |> List.concat_map @@ fun (use_domains, (lock_type, n_fibers)) ->
     if
       use_domains
       && (n_fibers = 1 || Picos_domain.recommended_domain_count () < n_fibers)
       || (is_ocaml4 && 256 < n_fibers)
     then []
     else run_one ~budgetf ~n_fibers ~use_domains ~lock_type ()
