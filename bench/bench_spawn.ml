open Multicore_bench
open Picos

let factor =
  Util.iter_factor
  * if String.starts_with ~prefix:"4." Sys.ocaml_version then 1 else 10

let run_one ~budgetf ~at_a_time () =
  let n_spawns = 10 * factor in

  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    let counter = ref n_spawns in
    let computation = Computation.create () in
    for _ = 1 to n_spawns / at_a_time do
      let main () =
        let n = !counter - 1 in
        counter := n;
        if n = 0 then Computation.finish computation
      in
      let mains = List.init at_a_time @@ fun _ -> main in
      Fiber.spawn ~forbid:false computation mains
    done;
    Computation.await computation
  in

  let config = Printf.sprintf "%d at a time" at_a_time in
  Times.record ~budgetf ~n_domains:1 ~n_warmups:1 ~n_runs_min:1 ~init ~wrap
    ~work ()
  |> Times.to_thruput_metrics ~n:n_spawns ~singular:"spawn" ~config

let run_suite ~budgetf =
  if Sys.int_size <= 32 then []
  else
    [ 1; 2; 4; 8 ]
    |> List.concat_map @@ fun at_a_time -> run_one ~budgetf ~at_a_time ()
