open Multicore_bench
open Picos
open Picos_sync
open Picos_structured

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

(** This will keep a domain running. *)
let yielder () =
  while true do
    Control.yield ()
  done

let n_workers = 4

let run_one ~budgetf ~use_domains ~n_resources () =
  let semaphore = Semaphore.Counting.make ~padded:true n_resources in

  let n_domains = if use_domains then n_workers else 1 in
  let n_ops =
    (if use_domains || is_ocaml4 then 10 else 100) * Util.iter_factor
  in

  let run_worker () =
    for _ = 1 to n_ops do
      Semaphore.Counting.acquire semaphore;
      Fiber.yield ();
      Semaphore.Counting.release semaphore
    done
  in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    Flock.join_after @@ fun () ->
    if use_domains then begin
      if not is_ocaml4 then Flock.fork yielder;
      run_worker ();
      Flock.terminate ()
    end
    else
      for _ = 1 to n_workers do
        Flock.fork run_worker
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
