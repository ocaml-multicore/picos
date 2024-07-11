open Multicore_bench
open Picos
open Picos_sync
open Picos_structured

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

(** This will keep a domain running. *)
let yielder computation =
  let main _ =
    try
      while true do
        Fiber.yield ()
      done
    with Exit -> ()
  in
  Fiber.spawn (Fiber.create ~forbid:false computation) main

let n_workers = 4

let run_one ~budgetf ~use_domains ~n_resources () =
  let semaphore = Semaphore.Counting.make ~padded:true n_resources in

  let n_domains = if use_domains then n_workers else 1 in
  let n_ops =
    (if use_domains || is_ocaml4 then 10 else 100) * Util.iter_factor
  in

  let run_worker () =
    let computation = Computation.create () in
    if not is_ocaml4 then yielder computation;
    for _ = 1 to n_ops do
      Semaphore.Counting.acquire semaphore;
      Fiber.yield ();
      Semaphore.Counting.release semaphore
    done;
    Computation.cancel computation (Exn_bt.get_callstack 0 Exit)
  in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    if use_domains then run_worker ()
    else
      Bundle.join_after @@ fun bundle ->
      for _ = 1 to n_workers do
        Bundle.fork bundle run_worker
      done
  in
  let config =
    Printf.sprintf "%d %s%s, %d resource%s" n_workers
      (if use_domains then "domain" else "fiber")
      (if n_workers = 1 then "" else "s")
      n_resources
      (if n_resources = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:(n_ops * n_workers) ~singular:"acquired yield"
       ~config

let run_suite ~budgetf =
  Util.cross [ false; true ] [ 1; 2; 3; 4 ]
  |> List.concat_map @@ fun (use_domains, n_resources) ->
     if use_domains && Picos_domain.recommended_domain_count () < n_workers then
       []
     else run_one ~budgetf ~use_domains ~n_resources ()
