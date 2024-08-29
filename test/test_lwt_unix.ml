open Picos_std_structured

let basics () =
  Lwt_main.run @@ Picos_lwt_unix.run
  @@ fun () ->
  Flock.join_after @@ fun () ->
  let child =
    Flock.fork_as_promise @@ fun () ->
    while true do
      Picos_lwt.await (Lwt_unix.sleep 0.01)
    done
  in
  Promise.terminate_after ~seconds:0.05 child

let () =
  [ ("Basics", [ Alcotest.test_case "" `Quick basics ]) ]
  |> Alcotest.run "Picos_lwt"
