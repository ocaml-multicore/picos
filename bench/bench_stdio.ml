open Multicore_bench
open Picos_io

let run_one ~budgetf ~block_or_nonblock ~n_domains () =
  let block_size = 4096 in
  let n_blocks = match block_or_nonblock with `Block -> 1 | `Nonblock -> 16 in

  let init _ =
    let inn, out = Unix.pipe ~cloexec:true () in
    (inn, out, Bytes.create block_size, Bytes.create 1)
  in
  let wrap _ _ = Scheduler.run in
  let work _ (inn, out, block, byte) =
    begin
      match block_or_nonblock with
      | `Block -> ()
      | `Nonblock -> Unix.set_nonblock inn
    end;
    for _ = 1 to n_blocks do
      let n = Unix.write out block 0 block_size in
      assert (n = block_size);
      for _ = 1 to block_size do
        let n : int = Unix.read inn byte 0 1 in
        assert (n = 1)
      done
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
  Times.to_thruput_metrics
    ~n:(block_size * n_blocks * n_domains)
    ~singular ~config times

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
