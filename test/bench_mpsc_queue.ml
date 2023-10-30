open Foundation

let n_messages = 500_000

let average action =
  let n_times = 10 in
  let n_warmup = 3 in
  let total = ref 0.0 in
  for i = 1 to n_times + n_warmup do
    let start = Unix.gettimeofday () in
    action ();
    let elapsed = Unix.gettimeofday () -. start in
    if n_warmup < i then total := !total +. elapsed
  done;
  !total /. Float.of_int n_times

let bench_single label create push pop =
  let elapsed =
    average @@ fun () ->
    let queue = create () in
    for i = 1 to n_messages do
      push queue i;
      pop queue
    done
  in

  Printf.printf "%s  (one domain): %f per second\n%!" label
    (Float.of_int n_messages /. elapsed)

let bench_multi label create push try_pop n_producers =
  let n_messages = n_messages / n_producers in

  let elapsed =
    average @@ fun () ->
    let queue = create () in

    let others =
      List.init n_producers @@ fun _ ->
      Domain.spawn @@ fun () ->
      for i = 1 to n_messages do
        push queue i
      done
    in

    let rec consume n = if 0 < n then consume (Bool.to_int (try_pop queue)) in
    consume (n_messages * n_producers);

    List.iter Domain.join others
  in

  Printf.printf "%s (%d producers): %f per second\n%!" label n_producers
    (Float.of_int (n_messages * n_producers) /. elapsed)

let () =
  bench_single "Earth single " Mpsc_queue.create Mpsc_queue.enqueue (fun q ->
      Mpsc_queue.dequeue q |> ignore);
  bench_single "Saturn single" Saturn.Single_consumer_queue.create
    Saturn.Single_consumer_queue.push (fun q ->
      Saturn.Single_consumer_queue.pop q |> ignore);
  bench_single "Saturn queue " Saturn.Queue.create Saturn.Queue.push (fun q ->
      Saturn.Queue.pop q |> ignore);

  [ 1; 2 ]
  |> List.iter @@ fun n ->
     print_endline "";
     bench_multi "Earth single " Mpsc_queue.create Mpsc_queue.enqueue
       (fun q ->
         match Mpsc_queue.dequeue q with
         | _ -> true
         | exception Mpsc_queue.Empty -> false)
       n;
     bench_multi "Saturn single" Saturn.Single_consumer_queue.create
       Saturn.Single_consumer_queue.push
       (fun q -> Saturn.Single_consumer_queue.pop_opt q |> Option.is_some)
       n;
     bench_multi "Saturn queue " Saturn.Queue.create Saturn.Queue.push
       (fun q -> Saturn.Queue.pop_opt q |> Option.is_some)
       n
