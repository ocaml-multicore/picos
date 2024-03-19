open Multicore_bench
module Htbl = Picos_htbl

module Int = struct
  include Int

  let hash = Fun.id
end

let run_one ~budgetf ~n_domains ?(n_ops = 400 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create ~hashed_type:(module Int) () in

  if prepopulate then
    for _ = 1 to n_keys do
      let value = Random.bits () in
      let key = value mod n_keys in
      Htbl.try_add t key value |> ignore
    done;

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < percent_mem then begin
              begin
                match Htbl.find_exn t key with
                | _ -> ()
                | exception Not_found -> ()
              end;
              loop (n - 1)
            end
            else if op < limit_add then begin
              Htbl.try_add t key value |> ignore;
              loop (n - 1)
            end
            else begin
              Htbl.try_remove t key |> ignore;
              loop (n - 1)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 10; 50; 90 ] [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun (percent_mem, n_domains) ->
     run_one ~budgetf ~n_domains ~percent_mem ()
