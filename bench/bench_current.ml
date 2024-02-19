open Multicore_bench
open Picos

let run_one ~budgetf ~n_domains () =
  let n_ops = 100 * Util.iter_factor * n_domains in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ = Atomic.set n_ops_todo n_ops in
  let work _ () =
    Schedulers.Fifos.run ~forbid:false @@ fun () ->
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let _ : _ Fiber.t = Fiber.current () in
            loop (n - 1)
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"op" ~config

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains -> run_one ~budgetf ~n_domains ()
