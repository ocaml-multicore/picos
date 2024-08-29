open Picos
open Picos_std_finally

let run_in_fiber main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid:false computation in
  let main _ = Computation.capture computation main () in
  Fiber.spawn fiber main;
  Computation.await computation

let test_fls_basics =
  (* It is imperative to test with both float... *)
  let float_key = Fiber.FLS.create () in

  (* ...and with non float keys. *)
  let counter_key = Fiber.FLS.create () in
  let counter = Atomic.make 0 in

  fun () ->
    Test_scheduler.run ~max_domains:2 @@ fun () ->
    begin
      run_in_fiber @@ fun () ->
      let fiber = Fiber.current () in
      begin
        match Fiber.FLS.get_exn fiber float_key with
        | _ -> assert false
        | exception Fiber.FLS.Not_set -> ()
      end;
      Fiber.FLS.set fiber float_key 4.2;
      Fiber.FLS.set fiber counter_key (Atomic.fetch_and_add counter 1);
      assert (Fiber.FLS.get_exn fiber float_key = 4.2);
      assert (Fiber.FLS.get_exn fiber counter_key = 0)
    end;
    begin
      run_in_fiber @@ fun () ->
      let fiber = Fiber.current () in
      begin
        match Fiber.FLS.get fiber counter_key ~default:101 with
        | 101 -> ()
        | _ -> assert false
      end;
      Fiber.FLS.set fiber counter_key (Atomic.fetch_and_add counter 1);
      Fiber.FLS.set fiber float_key 7.6;
      assert (Fiber.FLS.get_exn fiber counter_key = 1);
      assert (Fiber.FLS.get_exn fiber float_key = 7.6)
    end

let test_trigger_basics () =
  Test_scheduler.run @@ fun () ->
  let trigger = Trigger.create () in
  let@ _ =
    finally Domain.join @@ fun () ->
    Domain.spawn @@ fun () -> Trigger.signal trigger
  in
  match Trigger.await trigger with
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let test_computation_basics () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel computation Exit (Printexc.get_callstack 2);
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
    Computation.cancel computation Exit (Printexc.get_callstack 2)
  in
  Alcotest.check_raises "should be canceled" Exit @@ fun () ->
  let _ : int = Computation.await computation in
  ()

let test_computation_tx () =
  let module Tx = Computation.Tx in
  let empty_bt = Printexc.get_callstack 0 in
  let n_case_1 = ref 0 and n_case_2 = ref 0 in
  let n = 1_000 in
  let deadline = Unix.gettimeofday () +. 60.0 in
  while (!n_case_1 < n || !n_case_2 < n) && Unix.gettimeofday () < deadline do
    let a = Computation.create () in
    let b = Computation.create () in
    let barrier = Atomic.make 2 in
    let either which =
      if which then begin
        finally Domain.join @@ fun () ->
        Domain.spawn @@ fun () ->
        let backoff = ref Backoff.default in
        Atomic.decr barrier;
        while Atomic.get barrier <> 0 do
          Domain.cpu_relax ()
        done;
        while
          Computation.is_running a
          && not
               (let tx = Tx.create () in
                Tx.try_return tx a 101
                && Tx.try_cancel tx b Exit empty_bt
                && Tx.try_commit tx)
        do
          backoff := Backoff.once !backoff
        done
      end
      else begin
        finally Domain.join @@ fun () ->
        let backoff = ref Backoff.default in
        Domain.spawn @@ fun () ->
        Atomic.decr barrier;
        while Atomic.get barrier <> 0 do
          Domain.cpu_relax ()
        done;
        while
          Computation.is_running a
          && not
               (let tx = Tx.create () in
                not
                  (Tx.try_return tx b 42
                  && Tx.try_cancel tx a Exit empty_bt
                  && Tx.try_commit tx))
        do
          backoff := Backoff.once !backoff
        done
      end
    in
    let which = Random.bool () in
    let@ _ = either which in
    let@ _ = either (not which) in
    while Computation.is_running a || Computation.is_running b do
      Domain.cpu_relax ()
    done;
    if
      Computation.peek a = Some (Ok 101)
      && Computation.peek b = Some (Error (Exit, empty_bt))
    then incr n_case_1
    else if
      Computation.peek a = Some (Error (Exit, empty_bt))
      && Computation.peek b = Some (Ok 42)
    then incr n_case_2
    else assert false
  done;
  if not (n <= !n_case_1 && n <= !n_case_2) then
    if 0 < !n_case_1 && 0 < !n_case_2 && deadline <= Unix.gettimeofday () then
      print_endline "deadline"
    else assert false

let test_cancel () =
  Alcotest.check_raises "should be canceled" Exit @@ fun () ->
  Test_scheduler.run ~max_domains:2 @@ fun () ->
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
  Computation.cancel computation Exit (Printexc.get_callstack 0)

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
  Computation.cancel_after computation ~seconds:0.01 Not_found
    (Printexc.get_callstack 0);
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
    ("Trigger", [ Alcotest.test_case "basics" `Quick test_trigger_basics ]);
    ( "Computation",
      [
        Alcotest.test_case "basics" `Quick test_computation_basics;
        Alcotest.test_case "tx" `Quick test_computation_tx;
        Alcotest.test_case "signals in order" `Quick
          test_computation_completion_signals_triggers_in_order;
      ] );
    ("Fiber.FLS", [ Alcotest.test_case "basics" `Quick test_fls_basics ]);
    ("Cancel", [ Alcotest.test_case "" `Quick test_cancel ]);
    ("Cancel after", [ Alcotest.test_case "" `Quick test_cancel_after ]);
  ]
  |> Alcotest.run "Picos"
