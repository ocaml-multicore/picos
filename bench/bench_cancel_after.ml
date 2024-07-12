open Multicore_bench
open Picos

let exit_exn_bt = Exn_bt.get_callstack 0 Exit
let seconds = 0.000_000_001 (* 1 ns *)

let run_round_trip ~budgetf ~n_domains () =
  let n_ops = if Sys.win32 then 100 else 5 * Util.iter_factor * n_domains in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ = Countdown.non_atomic_set n_ops_todo n_ops in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:10 in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            let computation = Computation.create () in
            Computation.cancel_after computation ~seconds exit_exn_bt;
            Computation.wait computation;
            loop (n - 1)
          end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~n_warmups:1 ~n_runs_min:1 ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"round-trip" ~config

let run_async ~budgetf ~n_domains () =
  let n_ops = if Sys.win32 then 1_000 else 50 * Util.iter_factor * n_domains in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ = Countdown.non_atomic_set n_ops_todo n_ops in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    let queue = Queue.create () in
    let exit = ref false in

    let rec awaiter () =
      match Queue.take_opt queue with
      | None -> if not !exit then awaiter (Fiber.yield ())
      | Some computation ->
          Computation.wait computation;
          awaiter ()
    in
    let computation = Computation.create () in
    Fiber.spawn ~forbid:false computation
      [ Computation.capture computation awaiter ];

    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:10 in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            let computation = Computation.create () in
            Queue.push computation queue;
            Computation.cancel_after computation ~seconds exit_exn_bt;
            loop (n - 1)
          end
          else work ()
        in
        loop n
      else begin
        exit := true;
        Computation.wait computation
      end
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"async round-trip" ~config

let run_suite ~budgetf =
  Util.cross [ run_round_trip; run_async ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (run, n_domains) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run ~budgetf ~n_domains ()
