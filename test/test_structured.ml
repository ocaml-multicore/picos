open Picos_std_event
open Picos_std_finally
open Picos_std_structured
open Picos_std_sync
module Mpscq = Picos_aux_mpscq

(** Helper to check that computation is restored *)
let check join_after ?callstack ?on_return scope =
  let open Picos in
  let fiber = Fiber.current () in
  let before = Fiber.get_computation fiber in
  let check_computation_was_scoped () =
    let after = Fiber.get_computation fiber in
    assert (before == after)
  in
  lastly check_computation_was_scoped @@ fun () ->
  join_after ?callstack ?on_return @@ fun bundle ->
  let during = Fiber.get_computation fiber in
  assert (before != during);
  scope bundle

let test_fork_after_terminate () =
  Test_scheduler.run @@ fun () ->
  check Flock.join_after @@ fun () ->
  Flock.terminate ();
  match Flock.fork (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Control.Terminate -> ()

let test_fork_after_escape () =
  Test_scheduler.run @@ fun () ->
  let escape = ref (Obj.magic ()) in
  begin
    check Bundle.join_after @@ fun bundle -> escape := bundle
  end;
  match Bundle.fork !escape (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Invalid_argument _ -> ()

let test_exception_in_child_terminates () =
  match
    Test_scheduler.run ~max_domains:2 @@ fun () ->
    let mutex = Mutex.create () in
    let condition = Condition.create () in
    let blocked = ref false in
    check Flock.join_after @@ fun () ->
    begin
      Flock.fork @@ fun () ->
      begin
        Mutex.protect mutex @@ fun () ->
        while not !blocked do
          Condition.wait condition mutex
        done
      end;
      raise Exit
    end;
    begin
      Mutex.protect mutex @@ fun () ->
      blocked := true;
      Condition.signal condition;
      while true do
        Condition.wait condition mutex
      done
    end
  with
  | () -> assert false
  | exception Exit -> ()

let test_cancelation_awaits_children () =
  Test_scheduler.run ~max_domains:3 @@ fun () ->
  let blocked = ref false in
  let slept = ref false in
  begin
    check Flock.join_after ~on_return:`Terminate @@ fun () ->
    begin
      Flock.fork @@ fun () ->
      begin
        check Flock.join_after @@ fun () ->
        begin
          Flock.fork @@ fun () ->
          try
            blocked := true;
            Control.block ()
          with exn ->
            begin
              Control.protect @@ fun () ->
              Control.sleep ~seconds:0.2;
              slept := true
            end;
            raise exn
        end
      end
    end;
    while not !blocked do
      Control.sleep ~seconds:0.01
    done
  end;
  assert !slept

let test_block_raises () =
  Test_scheduler.run @@ fun () ->
  match Control.protect Control.block with
  | () -> assert false
  | exception Invalid_argument _ -> ()

let test_block_raises_sys_error () =
  Test_scheduler.run @@ fun () ->
  let open Picos in
  let success = ref false in
  let finished = Trigger.create () in
  let computation = Computation.create () in
  let main _ =
    begin
      try Control.block () with Sys_error _ -> success := true
    end;
    Trigger.signal finished
  in
  Fiber.spawn (Fiber.create ~forbid:false computation) main;
  Control.sleep ~seconds:0.1;
  Computation.finish computation;
  Trigger.await finished |> ignore;
  assert !success

let test_termination_nests () =
  Test_scheduler.run ~max_domains:3 @@ fun () ->
  let semaphore = Semaphore.Binary.make false in
  check Flock.join_after ~on_return:`Terminate @@ fun () ->
  begin
    Flock.fork @@ fun () ->
    begin
      check Flock.join_after @@ fun () ->
      begin
        Flock.fork @@ fun () ->
        Semaphore.Binary.release semaphore;
        while true do
          Control.sleep ~seconds:1.0
        done
      end
    end
  end;
  Semaphore.Binary.acquire semaphore

let test_promise_cancelation_does_not_terminate () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  Flock.join_after @@ fun () ->
  let promise = Flock.fork_as_promise @@ fun () -> Control.block () in
  Control.yield ();
  Promise.terminate promise;
  Control.yield ()

let test_error_in_promise_terminates () =
  match
    Test_scheduler.run ~max_domains:2 @@ fun () ->
    Flock.join_after @@ fun () ->
    let promise = Flock.fork_as_promise @@ fun () -> failwith "I failed" in
    Control.block () |> ignore;
    Promise.terminate promise
  with
  | () -> assert false
  | exception Failure _ -> ()

let test_can_wait_promises () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  Flock.join_after @@ fun () ->
  let promise =
    Flock.fork_as_promise @@ fun () ->
    Control.sleep ~seconds:0.1;
    42
  in
  assert (Promise.await promise = 42)

let test_can_select_promises () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  Flock.join_after ~on_return:`Terminate @@ fun () ->
  let a =
    Flock.fork_as_promise @@ fun () ->
    Control.sleep ~seconds:0.1;
    42
  and b = Flock.fork_as_promise @@ fun () -> Event.select [] in
  assert (Event.select [ Promise.completed a; Promise.completed b ] = 42)

let test_any_and_all_errors () =
  [ Run.all; Run.any ]
  |> List.iter @@ fun run_op ->
     Test_scheduler.run ~max_domains:6 @@ fun () ->
     let raised = Mpscq.create ~padded:true () in
     let raiser exn () =
       Mpscq.push raised exn;
       raise exn
     in
     match
       let ops =
         Test_util.shuffle
           [
             (fun () -> raise Control.Terminate (* Not counted as an error *));
             raiser Not_found;
             (fun () -> Control.block ());
             raiser Exit;
             raiser Division_by_zero;
           ]
       in
       run_op ops
     with
     | () -> assert false
     | exception exn ->
         let exn_bts =
           match exn with
           | Control.Errors exn_bts -> exn_bts
           | exn ->
               let bt = Printexc.get_raw_backtrace () in
               [ (exn, bt) ]
         in
         Mpscq.pop_all raised
         |> Seq.iter @@ fun exn ->
            assert (exn_bts |> List.exists @@ fun (exn', _) -> exn == exn')

let test_any_and_all_returns () =
  [ 0; 1; 2 ]
  |> List.iter @@ fun n_terminates ->
     [ 0; 1; 2 ]
     |> List.iter @@ fun n_incr ->
        [ (Run.all, n_incr, n_incr); (Run.any, Int.min 1 n_incr, n_incr) ]
        |> List.iter @@ fun (run_op, min, max) ->
           Test_scheduler.run ~max_domains:(n_terminates + n_incr + 1)
           @@ fun () ->
           let count = Atomic.make 0 in
           let ops =
             List.init n_terminates (fun _ () ->
                 raise Control.Terminate (* Not counted as an error *))
             @ List.init n_incr (fun _ () -> Atomic.incr count)
             |> Test_util.shuffle
           in
           run_op ops;
           let n = Atomic.get count in
           assert (min <= n);
           assert (n <= max)

let test_race_any () =
  Test_scheduler.run ~max_domains:4 @@ fun () ->
  let winner = Atomic.make 0 in
  let ops =
    Test_util.shuffle
      [
        (fun () ->
          try Control.terminate_after ~seconds:2.9 Control.block
          with Control.Terminate ->
            Atomic.compare_and_set winner 0 3 |> ignore);
        (fun () ->
          Control.sleep ~seconds:1.5;
          Atomic.compare_and_set winner 0 2 |> ignore);
        (fun () ->
          try Control.terminate_after ~seconds:0.1 Control.block
          with Control.Terminate ->
            Atomic.compare_and_set winner 0 1 |> ignore);
      ]
  in
  Run.any ops;
  (* This is non-deterministic and may need to changed if flaky *)
  assert (Atomic.get winner = 1)

let () =
  [
    ( "Bundle",
      [
        Alcotest.test_case "fork after terminate" `Quick
          test_fork_after_terminate;
        Alcotest.test_case "fork after escape" `Quick test_fork_after_escape;
        Alcotest.test_case "exception in child terminates" `Quick
          test_exception_in_child_terminates;
        Alcotest.test_case "cancelation awaits children" `Quick
          test_cancelation_awaits_children;
        Alcotest.test_case "block raises when forbidden" `Quick
          test_block_raises;
        Alcotest.test_case "block raises Sys_error when fiber finishes" `Quick
          test_block_raises_sys_error;
        Alcotest.test_case "termination nests" `Quick test_termination_nests;
        Alcotest.test_case "promise cancelation does not terminate" `Quick
          test_promise_cancelation_does_not_terminate;
        Alcotest.test_case "error in promise terminates" `Quick
          test_error_in_promise_terminates;
        Alcotest.test_case "can wait promises" `Quick test_can_wait_promises;
        Alcotest.test_case "can select promises" `Quick test_can_select_promises;
        Alcotest.test_case "any and all errors" `Quick test_any_and_all_errors;
        Alcotest.test_case "any and all returns" `Quick test_any_and_all_returns;
        Alcotest.test_case "race any" `Quick test_race_any;
      ] );
  ]
  |> Alcotest.run "Picos_structured"
