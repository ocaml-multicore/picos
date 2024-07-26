open Multicore_bench
open Picos

let rec lin_fib n i0 i1 = if 0 < n then lin_fib (n - 1) i1 (i0 + i1) else i0
let lin_fib i = lin_fib i 0 1

let rec exp_fib i =
  if i < 2 then i
  else
    let computation = Computation.create ~mode:`LIFO () in
    let main () = Computation.return computation (exp_fib (i - 2)) in
    Fiber.spawn ~forbid:false computation [ main ];
    let f1 = exp_fib (i - 1) in
    let f2 = Computation.await computation in
    f1 + f2

let ratio = (1.0 +. Float.sqrt 5.0) *. 0.5

let run_one ~budgetf ~n_domains ~n () =
  let context = ref (Obj.magic ()) in

  let before _ = context := Picos_randos.context () in
  let init _ = !context in
  let work i context =
    if i <> 0 then Picos_randos.runner_on_this_thread context
    else ignore @@ Picos_randos.run ~context @@ fun () -> exp_fib n
  in

  let config =
    Printf.sprintf "%d rando%s, fib %d" n_domains
      (if n_domains = 1 then "" else "s")
      n
  in
  Times.record ~budgetf ~n_domains ~before ~init ~work ()
  |> Times.to_thruput_metrics
       ~n:(Float.to_int (Float.of_int (lin_fib n) *. ratio))
       ~singular:"spawn" ~config

let run_suite ~budgetf =
  let n_over_budget = ref 100 in
  Util.cross [ 1; 2; 4; 8 ] [ 20; 24; 28 ]
  |> List.concat_map @@ fun (n_domains, n) ->
     if
       Picos_domain.recommended_domain_count () < n_domains
       || Sys.int_size <= 32 || Sys.backend_type <> Native || !n_over_budget < n
     then []
     else
       let start = Unix.gettimeofday () in
       let results = run_one ~budgetf ~n_domains ~n () in
       if budgetf < 1.0 && budgetf *. 2.0 < Unix.gettimeofday () -. start then
         n_over_budget := Int.min n !n_over_budget;
       results
