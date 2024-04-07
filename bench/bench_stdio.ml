open Multicore_bench
open Picos_stdio

let run_one ~budgetf ~block_or_nonblock ~n_domains () =
  let n_bytes =
    match block_or_nonblock with `Block -> 4096 | `Nonblock -> 65536
  in

  let init _ =
    let inn, out = Unix.pipe ~cloexec:true () in
    (inn, out, Bytes.create 1)
  in
  let wrap _ _ = Scheduler.run in
  let work _ (inn, out, byte) =
    begin
      match block_or_nonblock with
      | `Block -> ()
      | `Nonblock -> Unix.set_nonblock inn
    end;
    let n = Unix.write out (Bytes.create n_bytes) 0 n_bytes in
    assert (n = n_bytes);
    for _ = 1 to n_bytes do
      let n : int = Unix.read inn byte 0 1 in
      assert (n = 1)
    done;
    Unix.close inn;
    Unix.close out
  in

  let times =
    Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap
      ~work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  and singular =
    match block_or_nonblock with
    | `Block -> "blocking read"
    | `Nonblock -> "non-blocking read"
  in
  Times.to_thruput_metrics ~n:(n_bytes * n_domains) ~singular ~config times

let run_suite ~budgetf =
  Util.cross [ `Nonblock; `Block ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (block_or_nonblock, n_domains) ->
     if
       Sys.win32
       || Picos_domain.recommended_domain_count () < n_domains
       || String.starts_with ~prefix:"5.0." Sys.ocaml_version
          && block_or_nonblock == `Block
     then []
     else run_one ~budgetf ~block_or_nonblock ~n_domains ()
