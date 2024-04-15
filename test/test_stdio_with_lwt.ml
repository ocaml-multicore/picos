open Picos
open Picos_stdio

let () =
  let[@alert "-handler"] rec propagate () =
    let computation =
      Computation.with_action () () @@ fun _ _ _ ->
      Lwt_unix.handle_signal Sys.sigchld;
      propagate ()
    in
    Picos_select.return_on_sigchld computation ()
  in
  propagate ()

let test_system_unix () =
  let sleep = Lwt_unix.system "sleep 2" in
  Lwt_main.run @@ Lwt.bind sleep
  @@ fun _status ->
  Test_scheduler.run @@ fun () ->
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
  |> Alcotest.run "Picos_stdio_with_lwt"
