open Multicore_bench

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Picos_mpmcq.create ~padded:true () in

  let op push =
    if push then Picos_mpmcq.push t 101
    else
      match Picos_mpmcq.pop_exn t with
      | _ -> ()
      | exception Picos_mpmcq.Empty -> ()
  in

  let init _ =
    assert (
      match Picos_mpmcq.pop_exn t with
      | _ -> false
      | exception Picos_mpmcq.Empty -> true);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ~n_adders ~n_takers () =
  let n_domains = n_adders + n_takers in

  let n_msgs = 50 * Util.iter_factor in

  let t = Picos_mpmcq.create ~padded:true () in

  let n_msgs_to_add = Countdown.create ~n_domains:n_adders () in
  let n_msgs_to_take = Countdown.create ~n_domains:n_takers () in

  let init _ =
    assert (
      match Picos_mpmcq.pop_exn t with
      | _ -> false
      | exception Picos_mpmcq.Empty -> true);
    Countdown.non_atomic_set n_msgs_to_add n_msgs;
    Countdown.non_atomic_set n_msgs_to_take n_msgs
  in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Countdown.alloc n_msgs_to_add ~domain_index:i ~batch:1000 in
        if 0 < n then begin
          for i = 1 to n do
            Picos_mpmcq.push t i
          done;
          work ()
        end
      in
      work ()
    else
      let i = i - n_adders in
      let rec work () =
        let n = Countdown.alloc n_msgs_to_take ~domain_index:i ~batch:1000 in
        if 0 < n then
          let rec loop n =
            if 0 < n then begin
              match Picos_mpmcq.pop_exn t with
              | _ -> loop (n - 1)
              | exception Picos_mpmcq.Empty ->
                  Backoff.once Backoff.default |> ignore;
                  loop n
            end
            else work ()
          in
          loop n
      in
      work ()
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s"
      (format "nb adder" n_adders)
      (format "nb taker" n_takers)
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2; 4 ] [ 1; 2; 4 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       if Picos_domain.recommended_domain_count () < n_adders + n_takers then []
       else run_one ~budgetf ~n_adders ~n_takers ())
