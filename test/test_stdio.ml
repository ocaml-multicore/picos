open Picos_stdio

let test_system_unix () =
  Test_scheduler.run @@ fun () ->
  assert (Unix.system "exit 101" = Unix.WEXITED 101);
  assert (Unix.system "echo Hello world!" = Unix.WEXITED 0);
  assert (Unix.system "this-is-not-supposed-to-exist" = Unix.WEXITED 127);
  match Unix.wait () with
  | _ -> assert false
  | exception Unix.Unix_error (ECHILD, _, _) -> ()

let () =
  [
    ( "Unix",
      if Sys.win32 then []
      else [ Alcotest.test_case "system" `Quick test_system_unix ] );
  ]
  |> Alcotest.run "Picos_stdio"
