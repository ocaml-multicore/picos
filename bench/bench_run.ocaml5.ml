open Multicore_bench
open Picos_std_structured
module Multififo = Picos_mux_multififo

let run_one_multififo ~budgetf ~n_domains ~n () =
  let context = ref (Obj.magic ()) in

  let before _ = context := Multififo.context () in
  let init _ = !context in
  let work i context =
    if i <> 0 then Multififo.runner_on_this_thread context
    else ignore @@ Multififo.run ~context @@ fun () -> Run.for_n n ignore
  in

  let config =
    Printf.sprintf "%d mfifo%s, run_n %d" n_domains
      (if n_domains = 1 then "" else "s")
      n
  in
  Times.record ~budgetf ~n_domains ~before ~init ~work ()
  |> Times.to_thruput_metrics ~n ~singular:"ignore" ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4; 8 ]
    [ 100; 1_000; 10_000; 100_000; 1_000_000; 10_000_000 ]
  |> List.concat_map @@ fun (n_domains, n) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one_multififo ~budgetf ~n_domains ~n ()
