# `Picos_fifos`

```ocaml
open Picos

let exit_exn_bt = Exn_bt.get_callstack 0 Exit
```

```ocaml
# Test_scheduler.run @@ fun () ->
  42
- : int = 42
```

## `current`

```ocaml
# Test_scheduler.run @@ fun () ->
  let fiber_parent = Fiber.current () in
  let fiber_child = ref fiber_parent in
  let computation = Computation.create () in
  Fiber.spawn ~forbid:false computation [ fun () ->
    Computation.cancel computation exit_exn_bt;
    fiber_child := Fiber.current () ];
  while fiber_parent == !fiber_child do
    Fiber.yield ()
  done
- : unit = ()
```

## `cancel_after`

```ocaml
# Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel_after computation ~seconds:0.1 exit_exn_bt;
  Computation.wait computation
- : unit = ()
```

```ocaml
# Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel_after computation ~seconds:10e10 exit_exn_bt
Exception:
Invalid_argument
 "Picos_select: seconds should be between 0 to pow(2, 53) nanoseconds".
```
