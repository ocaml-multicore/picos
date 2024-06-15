open Multicore_bench
open Picos
open Picos_sync
open Picos_structured

let run_one_domain ~budgetf () =
  let n_msgs = 200 * Util.iter_factor in
  let t = Ch.create () in
  let giver () =
    for i = 1 to n_msgs do
      Ch.give t i
    done
  and taker () =
    for _ = 1 to n_msgs do
      Ch.take t |> ignore
    done
  in
  let init _ = () in
  let wrap _ () = Scheduler.run in
  let work _ () =
    Bundle.join_after @@ fun bundle ->
    if Random.bool () then begin
      Bundle.fork bundle giver;
      Bundle.fork bundle taker
    end
    else begin
      Bundle.fork bundle taker;
      Bundle.fork bundle giver
    end
  in
  Times.record ~budgetf ~n_domains:1 ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ~n_givers ~n_takers () =
  let n_domains = n_givers + n_takers in

  let n_msgs = 200 / n_domains * Util.iter_factor in

  let t = Ch.create ~padded:true () in

  let n_msgs_to_give = Atomic.make 0 |> Multicore_magic.copy_as_padded in
  let n_msgs_to_take = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_msgs_to_give n_msgs;
    Atomic.set n_msgs_to_take n_msgs
  in
  let wrap _ () = Scheduler.run in
  let work i () =
    let yielder () =
      try
        while true do
          Fiber.yield ()
        done
      with Exit -> ()
    in
    let computation = Computation.create () in
    Fiber.spawn ~forbid:false computation [ yielder ];
    begin
      if i < n_givers then
        let rec work () =
          let n = Util.alloc n_msgs_to_give in
          if 0 < n then begin
            for i = 1 to n do
              Ch.give t i
            done;
            work ()
          end
        in
        work ()
      else
        let rec work () =
          let n = Util.alloc n_msgs_to_take in
          if 0 < n then begin
            for _ = 1 to n do
              Ch.take t |> ignore
            done;
            work ()
          end
        in
        work ()
    end;
    Computation.cancel computation (Exn_bt.get_callstack 0 Exit)
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s" (format "giver" n_givers) (format "taker" n_takers)
  in
  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2; 4 ] [ 1; 2; 4 ]
    |> List.concat_map @@ fun (n_givers, n_takers) ->
       if Picos_domain.recommended_domain_count () < n_givers + n_takers then []
       else run_one ~budgetf ~n_givers ~n_takers ())
