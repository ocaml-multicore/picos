open Multicore_bench
module Htbl = Picos_htbl

module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

let run_one ~budgetf ~n_domains ?(n_ops = 400 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create ~hashed_type:(module Key) () in

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let before () =
    let _ : _ Seq.t = Htbl.remove_all t in
    Atomic.set n_ops_todo n_ops
  in
  let init i =
    let state = Random.State.make_self_init () in
    if prepopulate then begin
      let n = ((i + 1) * n_keys / n_domains) - (i * n_keys / n_domains) in
      for _ = 1 to n do
        let value = Random.State.bits state in
        let key = value mod n_keys in
        Htbl.try_add t key value |> ignore
      done
    end;
    state
  in
  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then begin
        for _ = 1 to n do
          let value = Random.State.bits state in
          let op = (value asr 20) mod 100 in
          let key = value mod n_keys in
          if op < percent_mem then
            match Htbl.find_exn t key with _ -> () | exception Not_found -> ()
          else if op < limit_add then Htbl.try_add t key value |> ignore
          else Htbl.try_remove t key |> ignore
        done;
        work ()
      end
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
  in
  Times.record ~budgetf ~n_domains ~before ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 10; 50; 90 ] [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun (percent_mem, n_domains) ->
     run_one ~budgetf ~n_domains ~percent_mem ()
