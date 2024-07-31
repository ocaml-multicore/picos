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

let rec run ?(max_domains = 1) ?(allow_lwt = true) ?forbid main =
  let scheduler =
    match Random.int 3 with 0 -> `Fifos | 1 -> `Randos | _ -> `Lwt
  in
  match scheduler with
  | `Lwt ->
      if Picos_thread.is_main_thread () && allow_lwt then
        Lwt_main.run (Picos_lwt_unix.run ?forbid main)
      else run ~max_domains ~allow_lwt ?forbid main
  | `Randos ->
      let n_domains =
        Int.min max_domains (Domain.recommended_domain_count ())
      in
      Picos_randos.run_on ~n_domains ?forbid main
  | `Fifos -> Picos_fifos.run ?forbid main
