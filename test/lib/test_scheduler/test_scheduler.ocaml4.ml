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

let explain () = Printf.printf "Testing with scheduler: threads\n%!"

let run_fiber ?(verbose = false) ?max_domains:_ ?allow_lwt:_ ?avoid_threads:_
    ?fatal_exn_handler fiber main =
  init ();
  if verbose then explain ();
  try Picos_mux_thread.run_fiber ?fatal_exn_handler fiber main
  with exn ->
    if not verbose then explain ();
    raise exn

let run ?verbose ?max_domains ?allow_lwt ?avoid_threads ?fatal_exn_handler
    ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?verbose ?max_domains ?allow_lwt ?avoid_threads ?fatal_exn_handler
    fiber main;
  Computation.await computation
