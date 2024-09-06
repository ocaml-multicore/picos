open Picos_io

let test_system_unix () =
  Test_scheduler.init ();
  let sleep = Lwt_unix.system "sleep 2" in
  Lwt_main.run @@ Lwt.bind sleep
  @@ fun _status ->
  Test_scheduler.run ~allow_lwt:false @@ fun () ->
  assert (Unix.system "exit 101" = Unix.WEXITED 101);
  assert (Unix.system "echo Hello world!" = Unix.WEXITED 0);
  assert (Unix.system "this-is-not-supposed-to-exist" = Unix.WEXITED 127);
  match Unix.wait () with
  | _ -> assert false
  | exception Unix.Unix_error (ECHILD, _, _) ->
      Lwt.bind (Lwt_unix.system "ls -l") @@ fun _ -> Lwt.return ()

let () =
  [
    ( "Unix",
      if Sys.win32 then []
      else [ Alcotest.test_case "system" `Quick test_system_unix ] );
  ]
  |> Alcotest.run "Picos_io_with_lwt"
