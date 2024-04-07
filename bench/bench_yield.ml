open Multicore_bench
open Picos

let factor =
  Util.iter_factor
  * if String.starts_with ~prefix:"4." Sys.ocaml_version then 1 else 10

let run_one ~budgetf ~n_fibers () =
  let n_yields = 100 * factor in
  let n_yields_per_fiber = n_yields / n_fibers in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    let n_live = Atomic.make n_fibers in
    let computation = Computation.create () in
    List.init n_fibers (fun _ () ->
        for _ = 1 to n_yields_per_fiber do
          Fiber.yield ()
        done;
        if 1 = Atomic.fetch_and_add n_live (-1) then
          Computation.finish computation)
    |> Fiber.spawn ~forbid:false computation;
    Computation.wait computation
  in

  let config =
    Printf.sprintf "%d fiber%s" n_fibers (if n_fibers = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains:1 ~n_warmups:1 ~n_runs_min:1 ~init ~wrap
    ~work ()
  |> Times.to_thruput_metrics ~n:n_yields ~singular:"yield" ~config

let run_suite ~budgetf =
  [ 1; 10; 100; 1_000; 10_000 ]
  |> List.concat_map @@ fun n_fibers ->
     if n_fibers <= factor then run_one ~budgetf ~n_fibers () else []
