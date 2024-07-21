open Multicore_bench
open Picos

module Stash = struct
  type 'a t = { mutable size : int; array : 'a option array }

  let create ~capacity = { size = 0; array = Array.make capacity None }
  let capacity t = Array.length t.array
  let length t = t.size

  let unsafe_push t x =
    let x = Some x in
    let i = t.size in
    Array.unsafe_set t.array i x;
    t.size <- i + 1

  let unsafe_drop t i =
    let n = t.size - 1 in
    t.size <- n;
    let x = Array.unsafe_get t.array i in
    Array.unsafe_set t.array i (Array.unsafe_get t.array n);
    Array.unsafe_set t.array n None;
    match x with Some x -> x | None -> assert false
end

let run_one ~budgetf ~n_domains () =
  let n_ops = 200 / n_domains * Util.iter_factor in

  let computation = Computation.create () in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ =
    Countdown.non_atomic_set n_ops_todo n_ops;
    (Stash.create ~capacity:1000, Random.State.make_self_init ())
  in
  let work domain_index (triggers, state) =
    let rec work () =
      let n =
        Countdown.alloc n_ops_todo ~domain_index
          ~batch:(Stash.capacity triggers)
      in
      if 0 < n then
        let rec loop n =
          if 0 < n + Stash.length triggers then begin
            let bits = Random.State.bits state in

            if (bits land 1 = 0 && 0 < n) || 0 = Stash.length triggers then begin
              let trigger = Trigger.create () in
              Stash.unsafe_push triggers trigger;
              assert (Computation.try_attach computation trigger);
              loop (n - 1)
            end
            else begin
              let n_live = Stash.length triggers in
              let i = (bits lsr 1) mod n_live in
              let trigger = Stash.unsafe_drop triggers i in
              Computation.detach computation trigger;
              loop n
            end
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
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"attach detach pair" ~config

let run_trivial ~budgetf () =
  let n = 200 * Util.iter_factor in

  let computation = Computation.create () in

  let init _ = () in
  let work _ () =
    for _ = 1 to n do
      let trigger = Trigger.create () in
      assert (Computation.try_attach computation trigger);
      Computation.detach computation trigger
    done
  in

  let config = "trivial" in
  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n ~singular:"attach detach pair" ~config

let run_suite ~budgetf =
  run_trivial ~budgetf ()
  @ ([ 1; 2; 4 ]
    |> List.concat_map @@ fun n_domains ->
       if Picos_domain.recommended_domain_count () < n_domains then []
       else run_one ~budgetf ~n_domains ())
