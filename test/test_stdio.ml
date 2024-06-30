open Picos_structured.Finally
open Picos_structured
open Picos_stdio

let test_system_unix () =
  Test_scheduler.run @@ fun () ->
  assert (Unix.system "exit 101" = Unix.WEXITED 101);
  assert (Unix.system "echo Hello world!" = Unix.WEXITED 0);
  assert (Unix.system "this-is-not-supposed-to-exist" = Unix.WEXITED 127);
  match Unix.wait () with
  | _ -> assert false
  | exception Unix.Unix_error (ECHILD, _, _) -> ()

let test_openfile_and_read () =
  Test_scheduler.run @@ fun () ->
  let@ fd =
    finally Unix.close @@ fun () ->
    try Unix.openfile "test_stdio.ml" [ O_RDONLY ] 0o400
    with Unix.Unix_error (ENOENT, _, _) ->
      Unix.openfile "test/test_stdio.ml" [ O_RDONLY ] 0o400
  in
  let n = 10 in
  let bytes = Bytes.create n in
  assert (n = Unix.read fd bytes 0 n);
  assert (Bytes.to_string bytes = "open Picos")

let test_sleepf () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let start = Unix.gettimeofday () in
  Run.all (List.init 100 @@ fun _ () -> Unix.sleepf 0.01);
  let d = Unix.gettimeofday () -. start in
  (* This is non-deterministic and might need to be changed if flaky *)
  assert (0.01 <= d);
  (* This is non-deterministic and might need to be changed if flaky *)
  assert (d < 5.0)

let test_select_empty_timeout () =
  Test_scheduler.run @@ fun () ->
  let start = Unix.gettimeofday () in
  let _ = Unix.select [] [] [] 0.1 in
  let d = Unix.gettimeofday () -. start in
  (* This is non-deterministic and might need to be changed if flaky *)
  assert (0.1 <= d);
  (* This is non-deterministic and might need to be changed if flaky *)
  assert (d <= 5.0)

let test_select_empty_forever () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  begin
    Bundle.fork bundle @@ fun () ->
    let _ = Unix.select [] [] [] (-1.0) in
    Printf.printf "Impossible\n%!"
  end;
  Unix.sleepf 0.01;
  Bundle.terminate bundle

let test_select () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let@ msg_inn1, msg_out1 =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in
  let@ msg_inn2, msg_out2 =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in
  let@ syn_inn, syn_out =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in

  Unix.set_nonblock msg_inn1;
  Unix.set_nonblock msg_out1;
  Unix.set_nonblock msg_inn2;
  Unix.set_nonblock msg_out2;
  Unix.set_nonblock syn_inn;
  Unix.set_nonblock syn_out;

  let events = Picos_mpscq.create () in

  Bundle.join_after @@ fun bundle ->
  begin
    Bundle.fork bundle @@ fun () ->
    while true do
      match Unix.select [ msg_inn1; msg_inn2 ] [] [] 0.2 with
      | inns, _, _ ->
          if List.exists (( == ) msg_inn1) inns then begin
            Picos_mpscq.push events (Printf.sprintf "Inn1");
            assert (1 = Unix.read msg_inn1 (Bytes.create 1) 0 1);
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end;
          if List.exists (( == ) msg_inn2) inns then begin
            Picos_mpscq.push events (Printf.sprintf "Inn2");
            assert (1 = Unix.read msg_inn2 (Bytes.create 1) 0 1);
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end;
          if [] == inns then begin
            Picos_mpscq.push events (Printf.sprintf "Timeout");
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end
    done
  end;

  Control.yield ();
  assert (1 = Unix.write_substring msg_out1 "!" 0 1);
  Control.yield ();
  assert (1 = Unix.write_substring msg_out2 "!" 0 1);
  Control.yield ();
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

  Bundle.terminate bundle;

  (* This is non-deterministic and might need to be changed if flaky *)
  Alcotest.(check' (list string))
    ~msg:"events"
    ~actual:(Picos_mpscq.pop_all events |> List.of_seq)
    ~expected:[ "Inn1"; "Inn2"; "Timeout" ]

let () =
  [
    ( "Unix",
      let common_cases =
        [
          Alcotest.test_case "openfile and read" `Quick test_openfile_and_read;
          Alcotest.test_case "sleepf" `Quick test_sleepf;
          Alcotest.test_case "select empty timeout" `Quick
            test_select_empty_timeout;
          Alcotest.test_case "select empty ∞" `Quick test_select_empty_forever;
          Alcotest.test_case "select" `Quick test_select;
        ]
      in
      let win32_cases = [] in
      let unix_cases =
        [ Alcotest.test_case "system" `Quick test_system_unix ]
      in
      common_cases @ if Sys.win32 then win32_cases else unix_cases );
  ]
  |> Alcotest.run "Picos_stdio"
