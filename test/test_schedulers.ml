open Picos

let exit_exn_bt = Exn_bt.get_callstack 0 Exit

let test_returns () =
  let actual = Test_scheduler.run @@ fun () -> 42 in
  assert (actual = 42)

let test_current () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let fiber_parent = Fiber.current () in
  let fiber_child = ref fiber_parent in
  let computation = Computation.create () in
  Fiber.spawn ~forbid:false computation
    [
      (fun () ->
        Computation.cancel computation exit_exn_bt;
        fiber_child := Fiber.current ());
    ];
  while fiber_parent == !fiber_child do
    Fiber.yield ()
  done

let test_cancel_after_basic () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel_after computation ~seconds:0.1 exit_exn_bt;
  Computation.wait computation

let test_cancel_after_long_timeout () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  match Computation.cancel_after computation ~seconds:10e10 exit_exn_bt with
  | () -> Computation.finish computation
  | exception Invalid_argument _ -> ()

let () =
  [
    ("Returns", [ Alcotest.test_case "" `Quick test_returns ]);
    ("Current", [ Alcotest.test_case "" `Quick test_current ]);
    ( "Cancel_after",
      [
        Alcotest.test_case "basic" `Quick test_cancel_after_basic;
        Alcotest.test_case "long timeout" `Quick test_cancel_after_long_timeout;
      ] );
  ]
  |> Alcotest.run "Picos schedulers"
