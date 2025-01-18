let () = Printexc.record_backtrace true
let () = Random.self_init ()

open Picos

let init = ref false

let init () =
  if not !init then begin
    init := true;

    Picos_io_select.check_configured ();

    let[@alert "-handler"] rec propagate () =
      let computation =
        Picos.Computation.with_action () () @@ fun _ _ _ ->
        (* Note that [handle_signal] is documented to be "thread-safe". *)
        Lwt_unix.handle_signal Sys.sigchld;
        propagate ()
      in
      Picos_io_select.return_on_sigchld computation ()
    in
    propagate ()
  end

let run_fiber ?max_domains:_ ?allow_lwt:_ ?fatal_exn_handler fiber main =
  init ();
  Picos_mux_thread.run_fiber ?fatal_exn_handler fiber main

let run ?max_domains ?allow_lwt ?fatal_exn_handler ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?max_domains ?allow_lwt ?fatal_exn_handler fiber main;
  Computation.await computation
