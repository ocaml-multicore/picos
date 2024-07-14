let () = Printexc.record_backtrace true
let () = Random.self_init ()

open Picos

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

let run_fiber ?max_domains:_ ?allow_lwt:_ ?fatal_exn_handler fiber main =
  Picos_threaded.run_fiber ?fatal_exn_handler fiber main

let run ?max_domains ?allow_lwt ?fatal_exn_handler ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main fiber = Fiber.capture_and_finalize fiber computation main () in
  run_fiber ?max_domains ?allow_lwt ?fatal_exn_handler fiber main;
  Computation.await computation
