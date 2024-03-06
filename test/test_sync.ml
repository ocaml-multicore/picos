open Picos
open Picos_sync

module Fiber = struct
  include Fiber

  let start thunk =
    let computation = Computation.create () in
    Fiber.spawn ~forbid:false computation
      [ Computation.capture computation thunk ];
    computation

  let run thunk = Computation.await (start thunk)
end

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

let test_mutex_and_condition_basics () =
  Test_scheduler.run @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in

  [ None; Some false ]
  |> List.iter @@ fun checked ->
     [ Computation.create (); Computation.finished ]
     |> List.iter @@ fun computation ->
        [ Condition.signal; Condition.broadcast ]
        |> List.iter @@ fun release ->
           let n = Atomic.make 10 in
           let test = Computation.create () in

           let main () =
             Mutex.protect ?checked mutex (fun () ->
                 Condition.wait condition mutex);
             if 1 = Atomic.fetch_and_add n (-1) then Computation.finish test
           in
           Fiber.spawn ~forbid:false computation
             (List.init (Atomic.get n) @@ fun _ -> main);

           while Computation.is_running test do
             Fiber.yield ();
             release condition
           done

let test_mutex_and_condition_errors () =
  Test_scheduler.run @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  Mutex.protect mutex @@ fun () ->
  begin
    match Fiber.run (fun () -> Mutex.unlock mutex) with
    | () -> assert false
    | exception Sys_error _ -> ()
  end;
  begin
    match Fiber.run (fun () -> Condition.wait condition mutex) with
    | () -> assert false
    | exception Sys_error _ -> ()
  end

let test_mutex_and_condition_cancelation () =
  let mutex = Mutex.create () in
  let condition = Condition.create () in

  let step = ref 0 in

  let step_1 = Atomic.make 0 in
  let step_2 = Atomic.make 0 in
  let step_3 = Atomic.make 0 in
  let step_4 = Atomic.make 0 in
  let step_5 = Atomic.make 0 in

  let steps = [| step_1; step_2; step_3; step_4; step_5 |] in

  let exit_exn_bt = Exn_bt.get_callstack 0 Exit in

  let n = if 4 <= Domain.recommended_domain_count () - 2 then 4 else 2 in

  let computations =
    Array.init n @@ fun _ -> Computation.Packed (Computation.create ())
  in

  let attempt i finished ?checked () =
    computations.(i) <- Fiber.get_computation (Fiber.current ());
    match
      Atomic.incr step_1;
      Domain.cpu_relax ();
      Mutex.lock ?checked mutex
    with
    | () -> begin
        let n = !step in
        Atomic.incr step_2;
        Domain.cpu_relax ();
        step := n + 1;
        match Condition.wait condition mutex with
        | () ->
            let n = !step in
            Atomic.incr step_3;
            Domain.cpu_relax ();
            step := n + 1;
            Mutex.unlock ?checked mutex;
            Trigger.signal finished
        | exception _ ->
            let n = !step in
            Atomic.incr step_4;
            Domain.cpu_relax ();
            step := n + 1;
            Mutex.unlock ?checked mutex;
            Trigger.signal finished
      end
    | exception _ ->
        Atomic.incr step_5;
        Trigger.signal finished
  in

  let exit = Atomic.make n in

  let limit = if is_ocaml4 then 1_000 else 10_000 in

  let some_false = Some false in

  let main i () =
    Fun.protect ~finally:(fun () -> Atomic.decr exit) @@ fun () ->
    Test_scheduler.run @@ fun () ->
    let state = Random.State.make_self_init () in
    while Array.exists (fun step -> Atomic.get step < limit) steps do
      let finished = Trigger.create () in
      let checked = if Random.State.bool state then None else some_false in
      Fiber.spawn ~forbid:false (Computation.create ())
        [ attempt i finished ?checked ];
      Trigger.await finished |> ignore
    done
  in
  let domains =
    Domain.spawn (fun () ->
        let state = Random.State.make_self_init () in
        while Atomic.get exit <> 0 do
          Domain.cpu_relax ();
          let (Packed computation) =
            computations.(Random.State.bits state land (n - 1))
          in
          Computation.cancel computation exit_exn_bt
        done)
    :: List.init n (fun i -> Domain.spawn (main i))
  in
  let state = Random.State.make_self_init () in
  while Atomic.get exit <> 0 do
    Domain.cpu_relax ();
    if Random.State.bool state then Condition.broadcast condition
    else Condition.signal condition
  done;
  List.iter Domain.join domains;
  assert (!step = Atomic.get step_2 + Atomic.get step_3 + Atomic.get step_4);
  Array.iter (fun step -> assert (limit <= Atomic.get step)) steps

let test_lazy_basics () =
  Test_scheduler.run @@ fun () ->
  assert (101 = (Lazy.from_fun (fun () -> 101) |> Lazy.force_val));
  assert (42 = (Lazy.from_val 40 |> Lazy.map (( + ) 2) |> Lazy.force));
  assert (42 = (Lazy.from_val 21 |> Lazy.map_val (( * ) 2) |> Lazy.force_val));
  let s = ref (Lazy.from_val 0) in
  s := Lazy.from_fun (fun () -> 1 + Lazy.force !s);
  match Lazy.force !s with
  | _ -> assert false
  | exception Stdlib.Lazy.Undefined -> ()

let test_lazy_cancelation () =
  Test_scheduler.run @@ fun () ->
  let susp ?to_signal ~to_await result =
    Lazy.from_fun @@ fun () ->
    Option.iter Trigger.signal to_signal;
    match Trigger.await to_await with
    | None -> result
    | Some exn_bt -> Exn_bt.raise exn_bt
  in
  assert (
    let to_await = Trigger.create () in
    let s = susp ~to_await 42 in
    let tried = Atomic.make false in
    let main () =
      match Lazy.force s with
      | _ -> assert false
      | exception Exit -> Atomic.set tried true
    in
    let computation = Computation.create () in
    Computation.cancel computation (Exn_bt.get_callstack 0 Exit);
    Fiber.spawn ~forbid:false computation [ main ];
    while not (Atomic.get tried) do
      Fiber.yield ()
    done;
    Trigger.signal to_await;
    (not (Lazy.is_val s)) && Lazy.force s = 42);

  let to_await = Trigger.create () in
  let to_signal = Trigger.create () in
  let s = susp ~to_signal ~to_await 101 in
  let c = Fiber.start @@ fun () -> Lazy.force s in
  Trigger.await to_signal |> ignore;
  Computation.cancel c (Exn_bt.get_callstack 0 Exit);
  match Lazy.force s with
  | _ -> assert false
  | exception Exit -> Trigger.signal to_await

let () =
  [
    ( "Mutex and Condition",
      [
        Alcotest.test_case "basics" `Quick test_mutex_and_condition_basics;
        Alcotest.test_case "errors" `Quick test_mutex_and_condition_errors;
        Alcotest.test_case "cancelation" `Quick
          test_mutex_and_condition_cancelation;
      ] );
    ( "Lazy",
      [
        Alcotest.test_case "basics" `Quick test_lazy_basics;
        Alcotest.test_case "cancelation" `Quick test_lazy_cancelation;
      ] );
  ]
  |> Alcotest.run "Picos_sync"
