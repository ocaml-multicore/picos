open Multicore_bench
open Picos_structured
open Picos_sync

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

(** This will keep a domain running. *)
let yielder () =
  while true do
    Control.yield ()
  done

let run_one ~budgetf ~n_fibers ~use_domains () =
  let n_domains = if use_domains then n_fibers else 1 in
  let n_ops = (if use_domains then 10 else 100) * Util.iter_factor in

  let v = ref 0 in
  let n_ops_todo = Countdown.create ~n_domains () in
  let mutex = Mutex.create ~padded:true () in

  let batch = if use_domains || n_fibers < 16 then 1000 else 100 in

  let init _ =
    assert (!v = 0);
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    Flock.join_after @@ fun () ->
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            Mutex.lock mutex;
            let x = !v in
            v := x + 1;
            Control.yield ();
            assert (!v = x + 1);
            v := x;
            Mutex.unlock mutex;
            loop (n - 1)
          end
          else work ()
        in
        loop n
    in
    if use_domains then begin
      Flock.fork yielder;
      work ();
      Flock.terminate ()
    end
    else
      for _ = 1 to n_fibers do
        Flock.fork work
      done
  in

  let config =
    Printf.sprintf "%d %s%s" n_fibers
      (if use_domains then "domain" else "fiber")
      (if n_fibers = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"locked yield" ~config

let run_suite ~budgetf =
  Util.cross [ false; true ] [ 1; 2; 4; 8; 256; 512; 1024 ]
  |> List.concat_map @@ fun (use_domains, n_fibers) ->
     if
       use_domains
       && (n_fibers = 1 || Picos_domain.recommended_domain_count () < n_fibers)
       || (is_ocaml4 && 256 < n_fibers)
     then []
     else run_one ~budgetf ~n_fibers ~use_domains ()
