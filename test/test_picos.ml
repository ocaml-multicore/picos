open Picos
open Picos_finally

let run_in_fiber main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid:false computation in
  let main _ = Computation.capture computation main () in
  Fiber.spawn fiber main;
  Computation.await computation

let test_fls_basics =
  (* It is imperative to test with both float... *)
  let float_key = Fiber.FLS.new_key (Constant 1.01) in

  (* ...and with non float keys. *)
  let answer_key = Fiber.FLS.new_key (Constant 42) in

  let counter_key =
    let counter = Atomic.make 0 in
    Fiber.FLS.new_key @@ Computed (fun () -> Atomic.fetch_and_add counter 1)
  in

  fun () ->
    Test_scheduler.run ~max_domains:2 @@ fun () ->
    let first =
      run_in_fiber @@ fun () ->
      let fiber = Fiber.current () in
      Alcotest.(check' int)
        ~msg:"constant" ~expected:42
        ~actual:(Fiber.FLS.get fiber answer_key);
      Fiber.FLS.set fiber answer_key 101;
      Alcotest.(check' int)
        ~msg:"updated" ~expected:101
        ~actual:(Fiber.FLS.get fiber answer_key);
      Fiber.FLS.get fiber counter_key
    in
    run_in_fiber @@ fun () ->
    let fiber = Fiber.current () in
    Alcotest.(check' int)
      ~msg:"constant" ~expected:42
      ~actual:(Fiber.FLS.get fiber answer_key);
    Alcotest.(check' (float 0.0))
      ~msg:"constant" ~expected:1.01
      ~actual:(Fiber.FLS.get fiber float_key);
    Fiber.FLS.set fiber float_key 4.2;
    Alcotest.(check' (float 0.0))
      ~msg:"constant" ~expected:4.2
      ~actual:(Fiber.FLS.get fiber float_key);
    Alcotest.(check' int)
      ~msg:"computed" ~expected:(first + 1)
      ~actual:(Fiber.FLS.get fiber counter_key)

let test_trigger_basics () =
  Test_scheduler.run @@ fun () ->
  let trigger = Trigger.create () in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn @@ fun () -> Trigger.signal trigger
  in
  Trigger.await trigger |> Option.iter Exn_bt.raise

let test_computation_basics () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel computation (Exn_bt.get Exit);
  Computation.wait computation;
  let computation = Computation.create () in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn @@ fun () ->
    let rec fib i =
      Computation.check computation;
      Thread.yield ();
      if i <= 1 then i else fib (i - 1) + fib (i - 2)
    in
    Computation.capture computation fib 80
  in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn @@ fun () ->
    Unix.sleepf 0.01;
    Computation.cancel computation (Exn_bt.get_callstack 2 Exit)
  in
  Alcotest.check_raises "should be canceled" Exit @@ fun () ->
  let _ : int = Computation.await computation in
  ()

let test_thread_cancelation () =
  Alcotest.check_raises "should be canceled" Exit @@ fun () ->
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  let@ _ =
    finally Computation.await @@ fun () ->
    let result = Computation.create () in
    let main _ =
      Computation.capture result
        (fun () ->
          while true do
            Fiber.yield ()
          done)
        ()
    in
    Fiber.spawn (Fiber.create ~forbid:false computation) main;
    result
  in
  Computation.cancel computation (Exn_bt.get_callstack 0 Exit)

let test_cancel_after () =
  Alcotest.check_raises "should be canceled" Not_found @@ fun () ->
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let computation = Computation.create () in
  let main _ =
    Computation.capture computation
      (fun () ->
        while true do
          Fiber.yield ()
        done)
      ()
  in
  Fiber.spawn (Fiber.create ~forbid:false computation) main;
  Computation.cancel_after computation ~seconds:0.01
    (Exn_bt.get_callstack 0 Not_found);
  Computation.await computation

let test_computation_completion_signals_triggers_in_order () =
  [ `FIFO; `LIFO ]
  |> List.iter @@ fun mode ->
     let state = Random.State.make_self_init () in
     let num_non_trivial = ref 0 in
     for _ = 1 to 10 do
       let computation = Computation.create ~mode () in
       let signals = ref [] in
       let triggers = ref [] in
       let counter = ref 0 in
       let attach_one () =
         let i = !counter in
         counter := i + 1;
         let[@alert "-handler"] trigger =
           Trigger.from_action signals i @@ fun _ signals i ->
           signals := i :: !signals
         in
         triggers := trigger :: !triggers;
         assert (Computation.try_attach computation trigger)
       in
       let detach_one () =
         let n = List.length !triggers in
         if 0 < n then begin
           let bits = Random.State.bits state in
           let i = bits mod n in
           let trigger = List.nth !triggers i in
           triggers := List.filter (( != ) trigger) !triggers;
           Computation.detach computation trigger
         end
       in
       for _ = 1 to 10 do
         for _ = 1 to 10 do
           let bits = Random.State.bits state in
           if bits land 3 <= 2 then attach_one () else detach_one ()
         done;
         for _ = 1 to List.length !triggers / 3 do
           detach_one ()
         done
       done;
       if List.length !triggers >= 2 then incr num_non_trivial;
       signals := [];
       Computation.finish computation;
       let expected =
         List.sort Int.compare !signals
         |> if mode = `FIFO then List.rev else Fun.id
       in
       assert (!signals = expected)
     done;
     assert (0 < !num_non_trivial)

let () =
  [
    ("Trigger basics", [ Alcotest.test_case "" `Quick test_trigger_basics ]);
    ( "Computation basics",
      [ Alcotest.test_case "" `Quick test_computation_basics ] );
    ("Fiber.FLS basics", [ Alcotest.test_case "" `Quick test_fls_basics ]);
    ( "Thread cancelation",
      [ Alcotest.test_case "" `Quick test_thread_cancelation ] );
    ("Cancel after", [ Alcotest.test_case "" `Quick test_cancel_after ]);
    ( "Computation signals in order",
      [
        Alcotest.test_case "" `Quick
          test_computation_completion_signals_triggers_in_order;
      ] );
  ]
  |> Alcotest.run "Picos"
