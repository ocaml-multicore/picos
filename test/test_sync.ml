open Picos
open Picos_sync
open Picos_structured

let msgs = ref []

(** Work around Stdlib.Domain not keeping stack traces. *)
module Domain = struct
  include Domain

  let () = Printexc.record_backtrace true

  let spawn main =
    Domain.spawn @@ fun () ->
    Printexc.record_backtrace true;
    match main () with
    | result -> Ok result
    | exception exn -> Error (Exn_bt.get exn)

  let join domain =
    match Domain.join domain with
    | Ok result -> result
    | Error exn_bt -> Exn_bt.raise exn_bt
end

module Fiber = struct
  include Fiber

  let start main =
    let computation = Computation.create ~mode:`LIFO () in
    let fiber = Fiber.create ~forbid:false computation in
    let main _ = Computation.capture computation main () in
    Fiber.spawn fiber main;
    computation
end

let test_mutex_and_condition_basics () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
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

           let main _ =
             Mutex.protect ?checked mutex (fun () ->
                 Condition.wait condition mutex);
             if 1 = Atomic.fetch_and_add n (-1) then Computation.finish test
           in
           for _ = 1 to Atomic.get n do
             Fiber.spawn (Fiber.create ~forbid:false computation) main
           done;

           while Computation.is_running test do
             Fiber.yield ();
             release condition
           done

let test_mutex_and_condition_errors () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  Mutex.protect mutex @@ fun () ->
  Flock.join_after @@ fun () ->
  begin
    Flock.fork @@ fun () ->
    match Mutex.unlock mutex with
    | () -> assert false
    | exception Sys_error _ -> ()
  end;
  begin
    Flock.fork @@ fun () ->
    match Condition.wait condition mutex with
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

  let attempt ?checked i fiber _ =
    let new_computation = Computation.Packed (Computation.create ()) in
    let old_computation = Fiber.get_computation fiber in
    Fiber.set_computation fiber new_computation;
    match
      computations.(i) <- new_computation;
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
            Fiber.set_computation fiber old_computation
        | exception _ ->
            let n = !step in
            Atomic.incr step_4;
            Domain.cpu_relax ();
            step := n + 1;
            Mutex.unlock ?checked mutex;
            Fiber.set_computation fiber old_computation
      end
    | exception _ ->
        Atomic.incr step_5;
        Fiber.set_computation fiber old_computation
  in

  let exit = Atomic.make n in

  let limit = 10_000 in

  let some_false = Some false in

  let deadline = Unix.gettimeofday () +. 60.0 in
  let hard_deadline = Unix.gettimeofday () +. 120.0 in

  let main i () =
    Fun.protect ~finally:(fun () -> Atomic.decr exit) @@ fun () ->
    Test_scheduler.run @@ fun () ->
    let state = Random.State.make_self_init () in
    let fiber = Fiber.current () in
    while
      Array.exists (fun step -> Atomic.get step < limit) steps
      && Unix.gettimeofday () < deadline
    do
      let checked = if Random.State.bool state then None else some_false in
      attempt ?checked i fiber ()
    done
  in
  let domains =
    Domain.spawn (fun () ->
        let state = Random.State.make_self_init () in
        while Atomic.get exit <> 0 do
          if hard_deadline < Unix.gettimeofday () then Stdlib.exit 4
          else Domain.cpu_relax ();
          let (Packed computation) =
            computations.(Random.State.bits state land (n - 1))
          in
          Computation.cancel computation exit_exn_bt
        done)
    :: List.init n (fun i -> Domain.spawn (main i))
  in
  let state = Random.State.make_self_init () in
  while Atomic.get exit <> 0 do
    if hard_deadline < Unix.gettimeofday () then Stdlib.exit 3
    else Domain.cpu_relax ();
    if Random.State.bool state then Condition.broadcast condition
    else Condition.signal condition
  done;
  List.iter Domain.join domains;
  assert (!step = Atomic.get step_2 + Atomic.get step_3 + Atomic.get step_4);
  if Array.exists (fun step -> Atomic.get step < limit) steps then begin
    let msg =
      steps |> Array.map Atomic.get
      |> Array.map (Printf.sprintf "%d")
      |> Array.to_list |> String.concat ", "
      |> Printf.sprintf "Mutex cancelation steps: [%s]"
    in
    msgs := msg :: !msgs
  end

let test_semaphore_basics () =
  Test_scheduler.run @@ fun () ->
  begin
    match Semaphore.Counting.make (-1) with
    | _ -> assert false
    | exception Invalid_argument _ -> ()
  end;
  begin
    let s = Semaphore.Counting.make Int.max_int in
    match Semaphore.Counting.release s with
    | () -> assert false
    | exception Sys_error _ ->
        Semaphore.Counting.acquire s;
        Semaphore.Counting.release s
  end

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

let test_semaphore_stress () =
  Test_scheduler.run ~max_domains:4 @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let s = Semaphore.Counting.make ~padded:true 0 in
  let rec loop ~n_acquire ~n_release =
    if 0 < n_acquire && 0 < n_release then
      if Random.bool () then fork_acquire ~n_acquire ~n_release
      else fork_release ~n_acquire ~n_release
    else if 0 < n_acquire then fork_acquire ~n_acquire ~n_release
    else if 0 < n_release then fork_release ~n_acquire ~n_release
  and fork_acquire ~n_acquire ~n_release =
    Bundle.fork bundle (fun () -> Semaphore.Counting.acquire s);
    loop ~n_acquire:(n_acquire - 1) ~n_release
  and fork_release ~n_acquire ~n_release =
    Bundle.fork bundle (fun () -> Semaphore.Counting.release s);
    loop ~n_acquire ~n_release:(n_release - 1)
  in
  let n = if is_ocaml4 then 100 else 100_000 in
  loop ~n_acquire:n ~n_release:n

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
  Test_scheduler.run ~max_domains:2 @@ fun () ->
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
    let main _ =
      match Lazy.force s with
      | _ -> assert false
      | exception Exit -> Atomic.set tried true
    in
    let computation = Computation.create () in
    Computation.cancel computation (Exn_bt.get_callstack 0 Exit);
    Fiber.spawn (Fiber.create ~forbid:false computation) main;
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

let test_event_basics () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  assert (
    Event.select
      [
        Event.from_computation (Computation.create ());
        Event.always 51 |> Event.map (( + ) 50);
      ]
    = 101);
  begin
    let c = Computation.create () in
    Computation.cancel_after c ~seconds:0.1 (Exn_bt.get_callstack 0 Not_found);
    match Event.sync (Event.from_computation c) with
    | () -> assert false
    | exception Not_found -> ()
  end;
  begin
    match
      [ Event.guard (fun () -> raise Exit); Event.always 42 ]
      |> Event.choose |> Event.sync
    with
    | _ -> assert false
    | exception Exit -> ()
  end

let () =
  try
    [
      ( "Mutex and Condition",
        [
          Alcotest.test_case "basics" `Quick test_mutex_and_condition_basics;
          Alcotest.test_case "errors" `Quick test_mutex_and_condition_errors;
          Alcotest.test_case "cancelation" `Quick
            test_mutex_and_condition_cancelation;
        ] );
      ( "Semaphore",
        [
          Alcotest.test_case "basics" `Quick test_semaphore_basics;
          Alcotest.test_case "stress" `Quick test_semaphore_stress;
        ] );
      ( "Lazy",
        [
          Alcotest.test_case "basics" `Quick test_lazy_basics;
          Alcotest.test_case "cancelation" `Quick test_lazy_cancelation;
        ] );
      ("Event", [ Alcotest.test_case "basics" `Quick test_event_basics ]);
    ]
    |> Alcotest.run ~and_exit:false "Picos_sync";
    !msgs |> List.iter (Printf.eprintf "%s\n%!")
  with Alcotest.Test_error -> exit 1
