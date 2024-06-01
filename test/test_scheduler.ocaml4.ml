let () = Printexc.record_backtrace true
let () = Random.self_init ()

let () =
  Picos_select.check_configured ();

  let[@alert "-handler"] rec propagate () =
    let computation =
      Picos.Computation.with_action () () @@ fun _ _ _ ->
      (* Note that [handle_signal] is documented to be "thread-safe". *)
      Lwt_unix.handle_signal Sys.sigchld;
      propagate ()
    in
    Picos_select.return_on_sigchld computation ()
  in
  propagate ()

let run ?max_domains:_ ?allow_lwt:_ ?forbid main =
  Picos_threaded.run ?forbid main
