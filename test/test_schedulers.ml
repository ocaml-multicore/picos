open Picos
open Picos_structured

let exit_exn_bt = Exn_bt.get_callstack 0 Exit

let test_returns () =
  let actual = Test_scheduler.run @@ fun () -> 42 in
  assert (actual = 42)

let test_completes () =
  let packed = ref (Computation.Packed (Computation.create ())) in
  let result =
    Test_scheduler.run (fun () ->
        packed := Fiber.get_computation (Fiber.current ());
        101)
  in
  assert (result = 101);
  let (Packed computation) = !packed in
  assert (not (Computation.is_running computation));
  assert (not (Computation.is_canceled computation));
  begin
    match
      Test_scheduler.run (fun () ->
          packed := Fiber.get_computation (Fiber.current ());
          (failwith "42" : unit))
    with
    | () -> assert false
    | exception Failure msg -> assert (msg = "42")
    | exception _ -> assert false
  end;
  let (Packed computation) = !packed in
  assert (not (Computation.is_running computation));
  assert (Computation.is_canceled computation)

let test_current () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let fiber_parent = Fiber.current () in
  let fiber_child = ref fiber_parent in
  Flock.join_after @@ fun () ->
  Flock.fork (fun () -> fiber_child := Fiber.current ());
  while fiber_parent == !fiber_child do
    Control.yield ()
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

let test_fatal () =
  match
    let computation = Computation.create () in
    let fiber = Fiber.create ~forbid:false computation in
    let fatal_exn_handler exn =
      Computation.cancel computation (Exn_bt.get exn);
      raise exn
    in
    Test_scheduler.run_fiber ~fatal_exn_handler ~max_domains:3 fiber @@ fun _ ->
    for _ = 1 to 100 do
      Fiber.spawn (Fiber.create ~forbid:false computation) @@ fun _ ->
      while true do
        Fiber.yield ()
      done
    done;
    Fiber.spawn (Fiber.create ~forbid:false computation) (fun _ ->
        failwith "fatal");
    while true do
      Fiber.yield ()
    done
  with
  | _ -> assert false
  | exception Failure msg -> assert (msg = "fatal")

let () =
  [
    ("Trivial main returns", [ Alcotest.test_case "" `Quick test_returns ]);
    ( "Scheduler completes main computation",
      [ Alcotest.test_case "" `Quick test_completes ] );
    ("Current", [ Alcotest.test_case "" `Quick test_current ]);
    ( "Cancel_after",
      [
        Alcotest.test_case "basic" `Quick test_cancel_after_basic;
        Alcotest.test_case "long timeout" `Quick test_cancel_after_long_timeout;
      ] );
    (* The fatal exn test must be kept last, because it may leave some
       schedulers in a bad state. *)
    ( "Fatal exception terminates scheduler",
      [ Alcotest.test_case "" `Quick test_fatal ] );
  ]
  |> Alcotest.run "Picos schedulers"
