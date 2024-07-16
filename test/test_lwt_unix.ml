open Picos_structured

let basics () =
  Lwt_main.run @@ Picos_lwt_unix.run
  @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let child =
    Bundle.fork_as_promise bundle @@ fun () ->
    while true do
      Picos_lwt.await (Lwt_unix.sleep 0.01)
    done
  in
  Promise.terminate_after ~seconds:0.05 child

let () =
  [ ("Basics", [ Alcotest.test_case "" `Quick basics ]) ]
  |> Alcotest.run "Picos_lwt"
