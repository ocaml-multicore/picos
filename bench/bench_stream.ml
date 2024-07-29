open Multicore_bench
module Stream = Picos_sync.Stream

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Stream.create ~padded:true () in

  let cursor = ref (Stream.tap t) in

  let op push =
    if push then Stream.push t 101
    else
      match Stream.peek_opt !cursor with
      | None -> ()
      | Some (_, c) -> cursor := c
  in

  let init _ =
    assert (Stream.peek_opt !cursor == None);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ~n_pusher () =
  let n_readers = 1 in
  let n_domains = n_pusher + n_readers in

  let n_msgs = 200 / n_readers * Util.iter_factor in

  let t = Stream.create ~padded:true () in

  let n_msgs_to_add = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_msgs_to_add n_msgs;
    Stream.tap t
  in
  let wrap _ _ = Scheduler.run in
  let work i c =
    if i < n_pusher then
      let rec work () =
        let n = Util.alloc n_msgs_to_add in
        if 0 < n then begin
          for i = 1 to n do
            Stream.push t i
          done;
          work ()
        end
      in
      work ()
    else
      let rec loop n c =
        if 0 < n then
          match Stream.peek_opt c with
          | None ->
              Backoff.once Backoff.default |> ignore;
              loop n c
          | Some (_, c) -> loop (n - 1) c
      in
      loop n_msgs c
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, 1 nb reader" (format "nb pusher" n_pusher)
  in
  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ ([ 1; 2; 4 ]
    |> List.concat_map @@ fun n_pusher ->
       if Picos_domain.recommended_domain_count () < n_pusher + 1 then []
       else run_one ~budgetf ~n_pusher ())
