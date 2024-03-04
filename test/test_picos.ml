open Picos
open Foundation.Finally

let run_in_domain thunk = Domain.join @@ Domain.spawn thunk

let test_dls_is_lazy =
  let counter = ref 0 in
  let key =
    DLS.new_key @@ fun () ->
    let v = !counter in
    counter := v + 1;
    v
  in
  fun () ->
    Alcotest.(check' int)
      ~msg:"must not be incremented" ~expected:0 ~actual:!counter;
    Alcotest.(check' int)
      ~msg:"must be initial" ~expected:0 ~actual:(DLS.get key);
    Alcotest.(check' int)
      ~msg:"must be incremented" ~expected:1 ~actual:!counter

let test_fls_basics =
  let answer_key = Fiber.FLS.new_key (Constant 42) in

  let counter_key =
    let counter = Atomic.make 0 in
    Fiber.FLS.new_key @@ Computed (fun () -> Atomic.fetch_and_add counter 1)
  in

  fun () ->
    let first =
      run_in_domain @@ fun () ->
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
    run_in_domain @@ fun () ->
    let fiber = Fiber.current () in
    Alcotest.(check' int)
      ~msg:"constant" ~expected:42
      ~actual:(Fiber.FLS.get fiber answer_key);
    Alcotest.(check' int)
      ~msg:"computed" ~expected:(first + 1)
      ~actual:(Fiber.FLS.get fiber counter_key)

let test_trigger_basics () =
  let trigger = Trigger.create () in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn (fun () -> Trigger.signal trigger)
  in
  Trigger.await trigger |> Option.iter Exn_bt.raise

let test_computation_basics () =
  let computation = Computation.create () in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn @@ fun () ->
    let rec fib i =
      Computation.check computation;
      Fiber.yield ();
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
  let computation = Computation.create () in
  let@ _ =
    finally Computation.await @@ fun () ->
    let result = Computation.create () in
    let main =
      Computation.capture result @@ fun () ->
      while true do
        Fiber.yield ()
      done
    in
    Fiber.spawn ~forbid:false computation [ main ];
    result
  in
  Computation.cancel computation (Exn_bt.get_callstack 0 Exit)

let test_cancel_after () =
  Alcotest.check_raises "should be canceled" Not_found @@ fun () ->
  let computation = Computation.create () in
  let main =
    Computation.capture computation @@ fun () ->
    while true do
      Fiber.yield ()
    done
  in
  Fiber.spawn ~forbid:false computation [ main ];
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
         let trigger = Trigger.create () in
         triggers := trigger :: !triggers;
         let i = !counter in
         counter := i + 1;
         assert (Computation.try_attach computation trigger);
         assert (
           Trigger.on_signal trigger () () (fun _ _ _ ->
               signals := i :: !signals))
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
    ("DLS is lazy", [ Alcotest.test_case "" `Quick test_dls_is_lazy ]);
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
