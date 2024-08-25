open Multicore_bench
open Picos

let key = Fiber.FLS.create ()

let run_one ~budgetf ~n_domains ~op () =
  let n_ops =
    (match op with `Get -> 800 | `Set -> 400) * Util.iter_factor * n_domains
  in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ = Countdown.non_atomic_set n_ops_todo n_ops in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    let fiber = Fiber.current () in
    Fiber.FLS.set fiber key (-1);
    match op with
    | `Get ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                let d = Fiber.FLS.get_exn fiber key in
                loop (n + d)
              end
              else work ()
            in
            loop n
        in
        work ()
    | `Set ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Fiber.FLS.set fiber key (-1);
                loop (n - 1)
              end
              else work ()
            in
            loop n
        in
        work ()
  in

  let singular = match op with `Get -> "get" | `Set -> "set" in
  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4 ] [ `Get; `Set ]
  |> List.concat_map @@ fun (n_domains, op) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~op ()
