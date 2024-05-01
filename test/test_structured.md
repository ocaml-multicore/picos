# `Picos_structured`

```ocaml
open Picos_structured.Finally
open Picos_structured
open Picos_sync
```

## Helper to check that computation is restored

```ocaml
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
```

## Fork after terminate raises

```ocaml
# Test_scheduler.run @@ fun () ->
  check Bundle.join_after @@ fun bundle ->
  Bundle.terminate bundle;
  Bundle.fork bundle (fun () -> Printf.printf "Hello!\n%!")
Exception: Picos_structured__Control.Terminate.
```

```ocaml
# Test_scheduler.run @@ fun () ->
  let escape = ref (Obj.magic ()) in
  check Bundle.join_after begin fun bundle ->
    escape := bundle;
  end;
  Bundle.fork !escape (fun () -> Printf.printf "Hello!\n%!")
Exception: Invalid_argument "already completed".
```

## Exception in child terminates bundle

```ocaml
# Test_scheduler.run @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let blocked = ref false in
  check Bundle.join_after @@ fun bundle ->
  Bundle.fork bundle begin fun () ->
    Mutex.protect mutex begin fun () ->
      while not !blocked do
        Condition.wait condition mutex
      done
    end;
    raise Exit
  end;
  Mutex.protect mutex begin fun () ->
    blocked := true;
    Printf.printf "Blocked\n%!";
    while true do
      Condition.wait condition mutex
    done
  end
Blocked
Exception: Stdlib.Exit.
```

## Termination (or cancelation) nests

```ocaml
# Test_scheduler.run @@ fun () ->
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let blocked = ref false in
  check Bundle.join_after begin fun bundle ->
    Bundle.fork bundle begin fun () ->
      check Bundle.join_after begin fun bundle ->
        Bundle.fork bundle begin fun () ->
          Mutex.protect mutex begin fun () ->
            blocked := true
          end;
          Printf.printf "Blocked\n%!";
          Condition.signal condition;
          while true do
             Control.sleep ~seconds:1.0;
          done
        end
      end
    end;

    Mutex.protect mutex begin fun () ->
      while not !blocked do
        Condition.wait condition mutex
      done
    end;

    Bundle.terminate bundle
  end
Blocked
- : unit = ()
```

## Cancelation also waits for children

```ocaml
# Test_scheduler.run @@ fun () ->
  let blocked = ref false in
  let slept = ref false in
  check Bundle.join_after begin fun bundle ->
    Bundle.fork bundle begin fun () ->
      check Bundle.join_after begin fun bundle ->
        Bundle.fork bundle begin fun () ->
          try
            blocked := true;
            Control.block ()
          with exn ->
            Control.protect begin fun () ->
              Printf.printf "Sleeping\n%!";
              Control.sleep ~seconds:0.2;
              Printf.printf "Woke up\n%!";
              slept := true;
            end;
            raise exn
        end
      end;
    end;
    while not !blocked do
      Control.sleep ~seconds:0.01
    done;
    Bundle.terminate bundle
  end;
  !slept
Sleeping
Woke up
- : bool = true
```

## Promise can be canceled without canceling the bundle

```ocaml
# Test_scheduler.run @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let promise =
    Bundle.fork_as_promise bundle @@ fun () ->
    Control.block ()
  in
  Control.yield ();
  Promise.terminate promise;
  Control.yield ();
  101
- : int = 101
```

## Error in promise ends the bundle

```ocaml
# Test_scheduler.run @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let promise =
    Bundle.fork_as_promise bundle @@ fun () ->
    failwith "I failed"
  in
  Control.block () |> ignore;
  Promise.terminate promise
Exception: Failure "I failed".
```

## Can wait for promises

```ocaml
# Test_scheduler.run @@ fun () ->
  Bundle.join_after @@ fun bundle ->
  let promise = Bundle.fork_as_promise bundle @@ fun () ->
    Control.sleep ~seconds:0.1;
    42
  in
  Promise.await promise
- : int = 42
```

## Racing

```ocaml
# Test_scheduler.run @@ fun () ->
  let winner = ref 0 in
  Run.any [
    (fun () -> Control.sleep ~seconds:0.9; winner := 3 );
    (fun () -> Control.sleep ~seconds:0.5; winner := 2 );
    (fun () -> Control.sleep ~seconds:0.1; winner := 1 );
  ];
  (* This is non-deterministic and may need to changed if flaky *)
  !winner
- : int = 1
```
