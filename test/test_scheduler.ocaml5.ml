let () = Printexc.record_backtrace true
let () = Random.self_init ()

open Picos

let () =
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

let rec run_fiber ?(max_domains = 1) ?(allow_lwt = true) ?fatal_exn_handler
    fiber main =
  let scheduler =
    match Random.int 4 with
    | 0 -> `Fifos
    | 1 -> `Multififos
    | 2 -> `Randos
    | _ -> `Lwt
  in
  let n_domains = Int.min max_domains (Domain.recommended_domain_count ()) in
  let quota = 1 + Random.int 100 in
  match
    match scheduler with
    | `Lwt ->
        if Picos_thread.is_main_thread () && allow_lwt then
          Some
            (fun () ->
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
                  raise exn)
        else None
    | `Randos ->
        Some
          (fun () ->
            Picos_mux_random.run_fiber_on ?fatal_exn_handler ~n_domains fiber
              main)
    | `Fifos ->
        Some
          (fun () ->
            Picos_mux_fifo.run_fiber ~quota ?fatal_exn_handler fiber main)
    | `Multififos ->
        Some
          (fun () ->
            Picos_mux_multififo.run_fiber_on ~quota ?fatal_exn_handler
              ~n_domains fiber main)
  with
  | None -> run_fiber ~max_domains ~allow_lwt ?fatal_exn_handler fiber main
  | Some run -> begin
      try run ()
      with exn ->
        Printf.printf "Test_scheduler: %s ~quota:%d ~n_domains:%d\n%!"
          (match scheduler with
          | `Fifos -> "fifos"
          | `Multififos -> "multififos"
          | `Randos -> "randos"
          | `Lwt -> "lwt")
          quota n_domains;
        raise exn
    end

let run ?max_domains ?allow_lwt ?fatal_exn_handler ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?max_domains ?allow_lwt ?fatal_exn_handler fiber main;
  Computation.await computation
