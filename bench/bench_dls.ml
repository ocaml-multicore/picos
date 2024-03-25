open Multicore_bench

let key = Picos_domain.DLS.new_key (fun () -> -1)

let run_one ~budgetf ~n_domains ~op () =
  let n_ops =
    (match op with `Get -> 800 | `Set -> 400) * Util.iter_factor * n_domains
  in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ = Atomic.set n_ops_todo n_ops in
  let work _ () =
    match op with
    | `Get ->
        let rec work () =
          let n = Util.alloc n_ops_todo in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                let d = Picos_domain.DLS.get key in
                loop (n + d)
              end
              else work ()
            in
            loop n
        in
        work ()
    | `Set ->
        let rec work () =
          let n = Util.alloc n_ops_todo in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Picos_domain.DLS.set key (-1);
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
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4 ] [ `Get; `Set ]
  |> List.concat_map @@ fun (n_domains, op) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~op ()
