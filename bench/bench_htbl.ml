open Multicore_bench
module Htbl = Picos_htbl

let run_one ~budgetf ~n_domains ?(n_ops = 400 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_read () =
  let t = Htbl.create ~equal:Int.equal ~hash:Fun.id () in

  let n_ops = (100 + percent_read) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  for i = 0 to n_keys - 1 do
    Htbl.try_add t i i |> ignore
  done;

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
            if op < percent_read then begin
              begin
                match Htbl.find_exn t key with
                | _ -> ()
                | exception Not_found -> ()
              end;
              loop (n - 1)
            end
            else begin
              Htbl.try_remove t key |> ignore;
              Htbl.try_add t key value |> ignore;
              loop (n - 2)
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
      percent_read
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 90; 50; 10 ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (percent_read, n_domains) ->
     run_one ~budgetf ~n_domains ~percent_read ()
