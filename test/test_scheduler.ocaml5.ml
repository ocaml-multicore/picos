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

let rec run_fiber ?(max_domains = 1) ?(allow_lwt = true) ?fatal_exn_handler
    fiber main =
  let scheduler =
    match Random.int 3 with 0 -> `Fifos | 1 -> `Randos | _ -> `Lwt
  in
  ignore
    (match scheduler with
    | `Fifos -> "fifos"
    | `Randos -> "randos"
    | `Lwt -> "lwt");
  match scheduler with
  | `Lwt ->
      if Picos_thread.is_main_thread () && allow_lwt then begin
        let old_hook = !Lwt.async_exception_hook in
        begin
          match fatal_exn_handler with
          | None -> ()
          | Some hook -> Lwt.async_exception_hook := hook
        end;
        match Lwt_main.run (Picos_lwt_unix.run_fiber fiber main) with
        | result ->
            Lwt.async_exception_hook := old_hook;
            result
        | exception exn ->
            Lwt.async_exception_hook := old_hook;
            raise exn
      end
      else run_fiber ~max_domains ~allow_lwt fiber main
  | `Randos ->
      let n_domains =
        Int.min max_domains (Domain.recommended_domain_count ())
      in
      Picos_randos.run_fiber_on ?fatal_exn_handler ~n_domains fiber main
  | `Fifos -> Picos_fifos.run_fiber ?fatal_exn_handler fiber main

let run ?max_domains ?allow_lwt ?fatal_exn_handler ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main fiber = Fiber.capture_and_finalize fiber computation main () in
  run_fiber ?max_domains ?allow_lwt ?fatal_exn_handler fiber main;
  Computation.await computation
