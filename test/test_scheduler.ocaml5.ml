open Picos

let () =
  Picos_select.check_configured ();

  let[@alert "-handler"] rec propagate () =
    let computation =
      Computation.with_action () () @@ fun _ _ _ ->
      Lwt_unix.handle_signal Sys.sigchld;
      propagate ()
    in
    Picos_select.return_on_sigchld computation ()
  in
  propagate ()

let fifos_run ?forbid main =
  Picos_fifos.run ?forbid @@ fun () ->
  if Picos_thread.is_main_thread () then
    Picos_select.reconfigure_signal_handlers ();
  main ()

let lwt_run ?forbid main =
  Lwt_main.run
  @@ Picos_lwt.run ?forbid ~sleep:Lwt_unix.sleep
  @@ fun () ->
  Picos_select.reconfigure_signal_handlers ();
  main ()

let run ?forbid ?(one_domain = false) main =
  let schedulers =
    if one_domain then [| lwt_run; fifos_run |] else [| fifos_run |]
  in
  let scheduler =
    let state = Random.State.make_self_init () in
    let i = Random.State.int state (Array.length schedulers) in
    schedulers.(i land 0)
  in
  scheduler ?forbid main
