open Picos
open Picos_std_event
open Picos_std_finally
open Picos_std_structured
open Picos_std_sync

let msgs = ref []
let empty_bt = Printexc.get_callstack 0

module Fiber = struct
  include Fiber

  let start main =
    let computation = Computation.create ~mode:`LIFO () in
    let fiber = Fiber.create ~forbid:false computation in
    let main _ = Computation.capture computation main () in
    Fiber.spawn fiber main;
    computation
end

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

module Make_mutex_and_condition_tests (Mutex : sig
  type t

  val create : ?padded:bool -> unit -> t
  val lock : ?checked:bool -> t -> unit
  val unlock : ?checked:bool -> t -> unit
  val protect : ?checked:bool -> t -> (unit -> 'a) -> 'a
end) (Condition : sig
  type t

  val create : ?padded:bool -> unit -> t
  val wait : t -> Mutex.t -> unit
  val signal : t -> unit
  val broadcast : t -> unit
end) =
struct
  let test_basics () =
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

  let test_cancelation () =
    Test_scheduler.init ();
    let mutex = Mutex.create () in
    let condition = Condition.create () in

    let step = ref 0 in

    let step_1 = Atomic.make 0 in
    let step_2 = Atomic.make 0 in
    let step_3 = Atomic.make 0 in
    let step_4 = Atomic.make 0 in
    let step_5 = Atomic.make 0 in

    let steps = [| step_1; step_2; step_3; step_4; step_5 |] in

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
      lastly (fun () -> Atomic.decr exit) @@ fun () ->
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
            Computation.cancel computation Exit empty_bt
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
end

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

module Make_semaphore_tests (Counting : sig
  include module type of Semaphore.Counting

  val max_value : int
end) =
struct
  let test_basics () =
    Test_scheduler.run @@ fun () ->
    begin
      match Counting.make (-1) with
      | _ -> assert false
      | exception Invalid_argument _ -> ()
    end;
    begin
      let s = Counting.make Counting.max_value in
      match Counting.release s with
      | () -> assert false
      | exception Sys_error _ ->
          assert (Counting.get_value s = Counting.max_value);
          Counting.acquire s;
          Counting.release s
    end

  let test_stress () =
    Test_scheduler.run ~max_domains:4 @@ fun () ->
    Bundle.join_after @@ fun bundle ->
    let s = Counting.make ~padded:true 0 in
    let rec loop ~n_acquire ~n_release =
      if 0 < n_acquire && 0 < n_release then
        if Random.bool () then fork_acquire ~n_acquire ~n_release
        else fork_release ~n_acquire ~n_release
      else if 0 < n_acquire then fork_acquire ~n_acquire ~n_release
      else if 0 < n_release then fork_release ~n_acquire ~n_release
    and fork_acquire ~n_acquire ~n_release =
      Bundle.fork bundle (fun () -> Counting.acquire s);
      loop ~n_acquire:(n_acquire - 1) ~n_release
    and fork_release ~n_acquire ~n_release =
      Bundle.fork bundle (fun () -> Counting.release s);
      loop ~n_acquire ~n_release:(n_release - 1)
    in
    let n = if is_ocaml4 then 100 else 100_000 in
    loop ~n_acquire:n ~n_release:n
end

module Semaphore_tests = Make_semaphore_tests (struct
  include Semaphore.Counting

  let max_value = Int.max_int
end)

module Sem_tests = struct
  include Make_semaphore_tests (struct
    include Sem

    let make = create
  end)

  let test_poisoning () =
    Test_scheduler.run ~max_domains:4 @@ fun () ->
    begin
      let sem = Sem.create 2 in
      Sem.acquire sem;
      Flock.join_after @@ fun () ->
      Sem.acquire sem;
      for _ = 0 to Random.int 3 do
        Flock.fork @@ fun () ->
        match Sem.acquire sem with
        | () -> assert false
        | exception Sem.Poisoned -> ()
      done;
      Sem.poison sem;
      assert (Sem.is_poisoned sem);
      assert (Sem.get_value sem = 0);
      Sem.release sem;
      assert (Sem.get_value sem = 0);
      assert (Sem.is_poisoned sem)
    end;
    ()

  let test_try_acquire () =
    Test_scheduler.run @@ fun () ->
    begin
      let sem = Sem.create 2 in
      assert (Sem.try_acquire sem);
      assert (Sem.try_acquire sem);
      assert (not (Sem.try_acquire sem));
      Sem.release sem;
      assert (Sem.try_acquire sem);
      Sem.poison sem;
      match Sem.try_acquire sem with
      | _ -> assert false
      | exception Sem.Poisoned -> ()
    end;
    ()
end

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
    | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
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
    Computation.cancel computation Exit empty_bt;
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
  Computation.cancel c Exit empty_bt;
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
    Computation.cancel_after c ~seconds:0.1 Not_found empty_bt;
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

let test_non_cancelable_ops () =
  Test_scheduler.run @@ fun () ->
  let ivar = Ivar.create () in
  let stream = Stream.create () in
  let latch = Latch.create 1 in
  let mutex = Mutex.create () in
  let counting = Semaphore.Counting.make 0 in
  let binary = Semaphore.Binary.make false in
  Flock.join_after @@ fun () ->
  Mutex.lock ~checked:(Random.bool ()) mutex;
  Flock.terminate ();
  try
    Ivar.poison ivar Exit;
    Stream.poison stream Exit;
    Latch.decr latch;
    Mutex.unlock ~checked:(Random.bool ()) mutex;
    Semaphore.Counting.release counting;
    Semaphore.Binary.release binary
  with _ -> assert false

module Make_lock_tests (Lock : module type of Lock) = struct
  let test_poisoning () =
    Test_scheduler.run ~max_domains:4 @@ fun () ->
    begin
      let lock = Lock.create () in
      Flock.join_after @@ fun () ->
      Lock.acquire lock;
      for _ = 0 to Random.int 3 do
        Flock.fork @@ fun () ->
        match Lock.acquire lock with
        | () -> assert false
        | exception Lock.Poisoned -> ()
      done;
      Lock.poison lock;
      assert (Lock.is_locked lock);
      assert (Lock.is_poisoned lock);
      Lock.release lock;
      assert (Lock.is_locked lock);
      assert (Lock.is_poisoned lock)
    end;
    begin
      let lock = Lock.create () in
      let condition = Lock.Condition.create () in
      Flock.join_after @@ fun () ->
      begin
        Flock.fork @@ fun () ->
        match
          Lock.holding lock @@ fun () ->
          while true do
            Lock.Condition.wait condition lock
          done
        with
        | () -> assert false
        | exception Lock.Poisoned -> ()
      end;
      begin
        Flock.fork @@ fun () ->
        match
          Lock.protect lock @@ fun () ->
          while true do
            Lock.Condition.wait condition lock
          done
        with
        | () -> assert false
        | exception Lock.Poisoned -> ()
      end;
      begin
        match Lock.holding lock @@ fun () -> raise Exit with
        | () -> assert false
        | exception Exit -> ()
      end;
      Lock.Condition.broadcast condition;
      begin
        match Lock.acquire lock with
        | () -> assert false
        | exception Lock.Poisoned -> ()
      end;
      assert (Lock.is_locked lock);
      assert (Lock.is_poisoned lock)
    end;
    ()

  let test_try_acquire () =
    Test_scheduler.run @@ fun () ->
    begin
      let lock = Lock.create () in
      assert (Lock.try_acquire lock);
      assert (not (Lock.try_acquire lock));
      Lock.release lock;
      assert (Lock.try_acquire lock);
      Lock.poison lock;
      match Lock.try_acquire lock with
      | _ -> assert false
      | exception Lock.Poisoned -> ()
    end;
    ()
end

module Mutex_and_condition_tests =
  Make_mutex_and_condition_tests (Mutex) (Condition)

module Lock_and_condition_tests = struct
  include
    Make_mutex_and_condition_tests
      (struct
        include Lock

        let lock ?checked:_ = acquire
        let unlock ?checked:_ = release
        let protect ?checked:_ = holding
      end)
      (Lock.Condition)

  include Make_lock_tests (Lock)
end

module Rwlock_and_condition_tests = struct
  include
    Make_mutex_and_condition_tests
      (struct
        include Rwlock

        let lock ?checked:_ = acquire
        let unlock ?checked:_ = release
        let protect ?checked:_ = holding
      end)
      (Rwlock.Condition)

  include Make_lock_tests (Rwlock)

  let test_sharing () =
    Test_scheduler.run @@ fun () ->
    begin
      let lock = Rwlock.create () in
      Flock.join_after @@ fun () ->
      Rwlock.acquire lock;
      for _ = 0 to Random.int 3 do
        Flock.fork @@ fun () ->
        match Rwlock.acquire_shared lock with
        | () -> assert false
        | exception Rwlock.Poisoned -> ()
      done;
      Rwlock.poison lock;
      assert (not (Rwlock.is_locked_shared lock));
      assert (Rwlock.is_locked lock);
      assert (Rwlock.is_poisoned lock);
      Rwlock.release lock;
      assert (Rwlock.is_locked lock);
      assert (Rwlock.is_poisoned lock)
    end;
    begin
      let lock = Rwlock.create () in
      let condition = Rwlock.Condition.create () in
      Flock.join_after @@ fun () ->
      begin
        Flock.fork @@ fun () ->
        match
          Rwlock.sharing lock @@ fun () ->
          assert (Rwlock.is_locked_shared lock);
          while true do
            Rwlock.Condition.wait_shared condition lock
          done
        with
        | () -> assert false
        | exception Rwlock.Poisoned -> ()
      end;
      begin
        match Rwlock.holding lock @@ fun () -> raise Exit with
        | () -> assert false
        | exception Exit -> assert (Rwlock.is_poisoned lock)
      end;
      Rwlock.Condition.broadcast condition;
      begin
        match Rwlock.acquire lock with
        | () -> assert false
        | exception Rwlock.Poisoned -> ()
      end;
      begin
        match
          Rwlock.sharing lock @@ fun () ->
          while true do
            Rwlock.Condition.wait_shared condition lock
          done
        with
        | () -> assert false
        | exception Rwlock.Poisoned -> ()
      end;
      assert (not (Rwlock.is_locked_shared lock));
      assert (Rwlock.is_locked lock);
      assert (Rwlock.is_poisoned lock)
    end;
    ()

  let test_freezing () =
    Test_scheduler.run @@ fun () ->
    begin
      let rwlock = Rwlock.create () in
      Flock.join_after @@ fun () ->
      Rwlock.acquire_shared rwlock;
      for _ = 0 to Random.int 3 do
        Flock.fork @@ fun () ->
        match Rwlock.acquire rwlock with
        | () -> assert false
        | exception Rwlock.Frozen -> ()
      done;
      Rwlock.freeze rwlock;
      for _ = 0 to Random.int 3 do
        Flock.fork @@ fun () ->
        match Rwlock.acquire_shared rwlock with
        | () -> Rwlock.release_shared rwlock
        | (exception Rwlock.Frozen) | (exception Rwlock.Poisoned) ->
            assert false
      done
    end

  let test_try_acquire_shared () =
    Test_scheduler.run ~max_domains:2 @@ fun () ->
    begin
      let lock = Rwlock.create () in
      assert (Rwlock.try_acquire_shared lock);
      assert (Rwlock.try_acquire_shared lock);
      Rwlock.release_shared lock;
      Rwlock.release_shared lock;
      Rwlock.acquire lock;
      assert (not (Rwlock.try_acquire_shared lock));
      Rwlock.poison lock;
      match Rwlock.try_acquire_shared lock with
      | _ -> assert false
      | exception Rwlock.Poisoned -> ()
    end;
    begin
      let lock = Rwlock.create () in
      assert (Rwlock.try_acquire_shared lock);
      Rwlock.freeze lock;
      match Rwlock.try_acquire lock with
      | _ -> assert false
      | exception Rwlock.Frozen -> ()
    end;
    ()
end

module Rwlock_is_a_submodule_of_Lock : module type of Lock = Rwlock

let () =
  try
    [
      ( "Mutex and Condition",
        [
          Alcotest.test_case "basics" `Quick
            Mutex_and_condition_tests.test_basics;
          Alcotest.test_case "errors" `Quick test_mutex_and_condition_errors;
          Alcotest.test_case "cancelation" `Quick
            Mutex_and_condition_tests.test_cancelation;
        ] );
      ( "Lock and Lock.Condition",
        [
          Alcotest.test_case "basics" `Quick
            Lock_and_condition_tests.test_basics;
          Alcotest.test_case "cancelation" `Quick
            Lock_and_condition_tests.test_cancelation;
          Alcotest.test_case "poisoning" `Quick
            Lock_and_condition_tests.test_poisoning;
          Alcotest.test_case "try_acquire" `Quick
            Lock_and_condition_tests.test_try_acquire;
        ] );
      ( "Rwlock and Rwlock.Condition",
        [
          Alcotest.test_case "basics" `Quick
            Rwlock_and_condition_tests.test_basics;
          Alcotest.test_case "cancelation" `Quick
            Rwlock_and_condition_tests.test_cancelation;
          Alcotest.test_case "poisoning" `Quick
            Rwlock_and_condition_tests.test_poisoning;
          Alcotest.test_case "freezing" `Quick
            Rwlock_and_condition_tests.test_freezing;
          Alcotest.test_case "try_acquire" `Quick
            Rwlock_and_condition_tests.test_try_acquire;
          Alcotest.test_case "try_acquire_shared" `Quick
            Rwlock_and_condition_tests.test_try_acquire_shared;
          Alcotest.test_case "sharing" `Quick
            Rwlock_and_condition_tests.test_sharing;
        ] );
      ( "Semaphore",
        [
          Alcotest.test_case "basics" `Quick Semaphore_tests.test_basics;
          Alcotest.test_case "stress" `Quick Semaphore_tests.test_stress;
        ] );
      ( "Sem",
        [
          Alcotest.test_case "basics" `Quick Sem_tests.test_basics;
          Alcotest.test_case "stress" `Quick Sem_tests.test_stress;
          Alcotest.test_case "poisoning" `Quick Sem_tests.test_poisoning;
          Alcotest.test_case "try_acquire" `Quick Sem_tests.test_try_acquire;
        ] );
      ( "Lazy",
        [
          Alcotest.test_case "basics" `Quick test_lazy_basics;
          Alcotest.test_case "cancelation" `Quick test_lazy_cancelation;
        ] );
      ("Event", [ Alcotest.test_case "basics" `Quick test_event_basics ]);
      ( "Non-cancelable ops",
        [ Alcotest.test_case "are not canceled" `Quick test_non_cancelable_ops ]
      );
    ]
    |> Alcotest.run ~and_exit:false "Picos_sync";
    !msgs |> List.iter (Printf.eprintf "%s\n%!")
  with Alcotest.Test_error -> exit 1
