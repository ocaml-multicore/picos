open Picos
module Picos_lwt_unix = Picos_lwt.Make (Lwt_unix)

let basics () =
  Lwt_main.run
  @@ Picos_lwt_unix.run ~forbid:false
  @@ fun () ->
  let computation = Computation.create () in
  let child =
    Computation.capture computation @@ fun () ->
    while true do
      Picos_lwt_unix.await (fun () -> Lwt_unix.sleep 0.01)
    done
  in
  Fiber.spawn ~forbid:false computation [ child ];
  Computation.cancel_after computation ~seconds:0.05
    (Exn_bt.get_callstack 0 Exit);
  match Computation.await computation with
  | () -> assert false
  | exception Exit -> ()

let () =
  [ ("Basics", [ Alcotest.test_case "" `Quick basics ]) ]
  |> Alcotest.run "Picos_lwt"
