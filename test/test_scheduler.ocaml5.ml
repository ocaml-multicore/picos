let () = Printexc.record_backtrace true
let () = Random.self_init ()

open Picos_structured.Finally

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
      let context = Picos_randos.context () in
      let rec spawn n =
        if n <= 1 then Picos_randos.run ~context ?forbid main
        else
          let@ _ =
            finally Domain.join @@ fun () ->
            try
              Domain.spawn @@ fun () ->
              Picos_randos.runner_on_this_thread context
            with exn ->
              Picos_randos.run ~context Fun.id;
              raise exn
          in
          spawn (n - 1)
      in
      spawn (Int.min max_domains (Domain.recommended_domain_count ()))
  | `Fifos -> Picos_fifos.run ?forbid main
