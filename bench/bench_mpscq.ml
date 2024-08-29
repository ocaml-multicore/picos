open Multicore_bench
module Mpscq = Picos_aux_mpscq

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Mpscq.create ~padded:true () in

  let op push =
    if push then Mpscq.push t 101
    else match Mpscq.pop_exn t with _ -> () | exception Mpscq.Empty -> ()
  in

  let init _ =
    assert (
      match Mpscq.pop_exn t with _ -> false | exception Mpscq.Empty -> true);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ~n_adders () =
  let n_takers = 1 in
  let n_domains = n_adders + n_takers in

  let n_msgs = 200 * Util.iter_factor in

  let t = Mpscq.create ~padded:true () in

  let n_msgs_to_add = Countdown.create ~n_domains:n_adders () in

  let init _ =
    assert (
      match Mpscq.pop_exn t with _ -> false | exception Mpscq.Empty -> true);
    Countdown.non_atomic_set n_msgs_to_add n_msgs
  in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Countdown.alloc n_msgs_to_add ~domain_index:i ~batch:1000 in
        if 0 < n then begin
          for i = 1 to n do
            Mpscq.push t i
          done;
          work ()
        end
      in
      work ()
    else
      let rec loop n =
        if 0 < n then
          match Mpscq.pop_exn t with
          | _ -> loop (n - 1)
          | exception Mpscq.Empty ->
              Backoff.once Backoff.default |> ignore;
              loop n
      in
      loop n_msgs
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, 1 nb taker" (format "nb adder" n_adders)
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ ([ 1; 2; 4 ]
    |> List.concat_map @@ fun n_adders ->
       if Picos_domain.recommended_domain_count () < 1 + n_adders then []
       else run_one ~budgetf ~n_adders ())
