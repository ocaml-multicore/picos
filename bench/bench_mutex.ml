open Multicore_bench
open Picos
open Picos_sync

(** This will keep a domain running. *)
let yielder computation =
  let main () =
    try
      while true do
        Fiber.yield ()
      done
    with Exit -> ()
  in
  Fiber.spawn ~forbid:false computation [ main ]

let run_one ~budgetf ~n_fibers ~use_domains () =
  let n_domains = if use_domains then n_fibers else 1 in
  let n_ops = (if use_domains then 10 else 100) * Util.iter_factor in

  let v = ref 0 in
  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in
  let mutex = Mutex.create () in

  let init _ =
    assert (!v = 0);
    Atomic.set n_ops_todo n_ops
  in
  let wrap _ () = Scheduler.run in
  let work _ () =
    let n_live = Atomic.make (if use_domains then 1 else n_fibers) in
    let computation = Computation.create () in
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            Mutex.lock mutex;
            let x = !v in
            v := x + 1;
            Fiber.yield ();
            assert (!v = x + 1);
            v := x;
            Mutex.unlock mutex;
            loop (n - 1)
          end
          else work ()
        in
        loop n
      else if 1 = Atomic.fetch_and_add n_live (-1) then
        Computation.cancel computation (Exn_bt.get_callstack 0 Exit)
    in
    if use_domains then begin
      yielder computation;
      work ()
    end
    else begin
      List.init n_fibers (fun _ -> work)
      |> Fiber.spawn ~forbid:false computation;
      Computation.wait computation
    end
  in

  let config =
    Printf.sprintf "%d %s%s" n_fibers
      (if use_domains then "domain" else "fiber")
      (if n_fibers = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"locked yield" ~config

let run_suite ~budgetf =
  Util.cross [ false; true ] [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun (use_domains, n_fibers) ->
     if
       use_domains
       && (n_fibers = 1 || Picos_domain.recommended_domain_count () < n_fibers)
     then []
     else run_one ~budgetf ~n_fibers ~use_domains ()
