open Multicore_bench
open Picos

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

let factor =
  Util.iter_factor
  * if String.starts_with ~prefix:"4." Sys.ocaml_version then 1 else 10

let run_one ~budgetf () =
  let n_spawns = 10 * factor in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    let counter = ref n_spawns in
    let computation = Computation.create ~mode:`LIFO () in
    let computation_packed = Computation.Packed computation in
    let main fiber =
      let n = !counter - 1 in
      counter := n;
      if n = 0 then Computation.finish computation;
      Fiber.finalize fiber
    in
    for _ = 1 to n_spawns do
      let fiber = Fiber.create_packed ~forbid:false computation_packed in
      Fiber.spawn fiber main
    done;
    Computation.await computation
  in

  let config = "with packed computation" in
  Times.record ~budgetf ~n_domains:1 ~n_warmups:1 ~n_runs_min:1 ~init ~wrap
    ~work ()
  |> Times.to_thruput_metrics ~n:n_spawns ~singular:"spawn" ~config

let run_suite ~budgetf =
  if Sys.int_size <= 32 || is_ocaml4 then [] else run_one ~budgetf ()
