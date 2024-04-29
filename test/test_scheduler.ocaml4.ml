open Picos

let () =
  Picos_select.check_configured ();

  Lwt_unix.handle_signal Sys.sigchld;
  let[@alert "-handler"] rec propagate () =
    let computation =
      Computation.with_action () () @@ fun _ _ _ ->
      Lwt_unix.handle_signal Sys.sigchld;
      propagate ()
    in
    Picos_select.return_on_sigchld computation ()
  in
  propagate ()

let run ?forbid ?one_domain main =
  ignore one_domain;
  Picos_threaded.run ?forbid @@ fun () ->
  if Picos_thread.is_main_thread () then
    Picos_select.reconfigure_signal_handlers ();
  main ()
