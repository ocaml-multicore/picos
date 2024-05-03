open Picos
open Picos_structured
open Picos_sync

(** Helper to check that computation is restored *)
let check join_after scope =
  let open Picos in
  let fiber = Fiber.current () in
  let before = Fiber.get_computation fiber in
  let finally () =
    let after = Fiber.get_computation fiber in
    assert (before == after)
  in
  Fun.protect ~finally @@ fun () ->
  join_after @@ fun bundle ->
  let during = Fiber.get_computation fiber in
  assert (before != during);
  scope bundle

let test_fork_after_terminate () =
  Test_scheduler.run @@ fun () ->
  check Bundle.join_after @@ fun bundle ->
  Bundle.terminate bundle;
  match Bundle.fork bundle (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Control.Terminate -> ()

let test_fork_after_escape () =
  Test_scheduler.run @@ fun () ->
  let escape = ref (Obj.magic ()) in
  check Bundle.join_after
    begin
      fun bundle -> escape := bundle
    end;
  match Bundle.fork !escape (fun () -> Printf.printf "Hello!\n%!") with
  | () -> assert false
  | exception Invalid_argument _ -> ()

let test_exception_in_child_terminates () =
  match
    Test_scheduler.run @@ fun () ->
    let mutex = Mutex.create () in
    let condition = Condition.create () in
    let blocked = ref false in
    check Bundle.join_after @@ fun bundle ->
    Bundle.fork bundle
      begin
        fun () ->
          Mutex.protect mutex
            begin
              fun () ->
                while not !blocked do
                  Condition.wait condition mutex
                done
            end;
          raise Exit
      end;
    Mutex.protect mutex
      begin
        fun () ->
          blocked := true;
          while true do
            Condition.wait condition mutex
          done
      end
  with
  | () -> assert false
  | exception Exit -> ()

let test_cancelation_awaits_children () =
  Test_scheduler.run @@ fun () ->
  let blocked = ref false in
  let slept = ref false in
  begin
    check Bundle.join_after @@ fun bundle ->
    begin
      Bundle.fork bundle @@ fun () ->
      begin
        check Bundle.join_after @@ fun bundle ->
        begin
          Bundle.fork bundle @@ fun () ->
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
    done;
    Bundle.terminate bundle
  end;
  assert !slept

let test_termination_nests () =
  Test_scheduler.run @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let blocked = ref false in
  begin
    check Bundle.join_after @@ fun bundle ->
    begin
      Bundle.fork bundle @@ fun () ->
      begin
        check Bundle.join_after @@ fun bundle ->
        begin
          Bundle.fork bundle @@ fun () ->
          begin
            Mutex.protect mutex @@ fun () -> blocked := true
          end;
          Condition.signal condition;
          while true do
            Control.sleep ~seconds:1.0
          done
        end
      end
    end;

    begin
      Mutex.protect mutex @@ fun () ->
      while not !blocked do
        Condition.wait condition mutex
      done
    end;

    Bundle.terminate bundle
  end

let test_promise_cancelation_does_not_terminate () =
  Test_scheduler.run @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let promise = Bundle.fork_as_promise bundle @@ fun () -> Control.block () in
  Control.yield ();
  Promise.terminate promise;
  Control.yield ()

let test_error_in_promise_terminates () =
  match
    Test_scheduler.run @@ fun () ->
    Bundle.join_after @@ fun bundle ->
    let promise =
      Bundle.fork_as_promise bundle @@ fun () -> failwith "I failed"
    in
    Control.block () |> ignore;
    Promise.terminate promise
  with
  | () -> assert false
  | exception Failure _ -> ()

let test_can_wait_promises () =
  Test_scheduler.run @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let promise =
    Bundle.fork_as_promise bundle @@ fun () ->
    Control.sleep ~seconds:0.1;
    42
  in
  assert (Promise.await promise = 42)

let test_any_and_all_errors () =
  [ Run.all; Run.any ]
  |> List.iter @@ fun run_op ->
     Test_scheduler.run @@ fun () ->
     let raised = Picos_mpsc_queue.create () in
     let raiser exn () =
       Picos_mpsc_queue.push raised exn;
       raise exn
     in
     match
       run_op
         [
           raiser Not_found;
           (fun () -> Control.block ());
           raiser Exit;
           raiser Division_by_zero;
         ]
     with
     | () -> assert false
     | exception exn ->
         let exn_bts =
           match exn with
           | Control.Errors exn_bts -> exn_bts
           | exn -> [ Exn_bt.get exn ]
         in
         Picos_mpsc_queue.pop_all raised
         |> Seq.iter @@ fun exn ->
            assert (
              exn_bts
              |> List.exists @@ fun (exn_bt : Exn_bt.t) -> exn == exn_bt.exn)

let test_race_any () =
  Test_scheduler.run @@ fun () ->
  let winner = ref 0 in
  Run.any
    [
      (fun () ->
        Control.sleep ~seconds:0.9;
        winner := 3);
      (fun () ->
        Control.sleep ~seconds:0.5;
        winner := 2);
      (fun () ->
        Control.sleep ~seconds:0.1;
        winner := 1);
    ];
  (* This is non-deterministic and may need to changed if flaky *)
  assert (!winner = 1)

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
        Alcotest.test_case "termination nests" `Quick test_termination_nests;
        Alcotest.test_case "promise cancelation does not terminate" `Quick
          test_promise_cancelation_does_not_terminate;
        Alcotest.test_case "error in promise terminates" `Quick
          test_error_in_promise_terminates;
        Alcotest.test_case "can wait promises" `Quick test_can_wait_promises;
        Alcotest.test_case "any and all errors" `Quick test_any_and_all_errors;
        Alcotest.test_case "race any" `Quick test_race_any;
      ] );
  ]
  |> Alcotest.run "Picos_structured"
