# `Picos_stdio`

```ocaml
open Foundation.Finally
open Picos
open Picos_stdio
```

```ocaml os_type<>Win32
# Test_scheduler.run @@ fun () ->
  Unix.system "ls test_stdio.md"
test_stdio.md
- : Unix.process_status = Picos_stdio.Unix.WEXITED 0
```

```ocaml os_type<>Win32
# Test_scheduler.run @@ fun () ->
  let@ fd =
    finally Unix.close @@ fun () ->
    Unix.openfile "test_stdio.md" [ O_RDONLY ] 0o400
  in
  let n = 15 in
  let bytes = Bytes.create n in
  assert (n = Unix.read fd bytes 0 n);
  Bytes.to_string bytes
- : string = "# `Picos_stdio`"
```

```ocaml
# Test_scheduler.run @@ fun () ->
  let children = Computation.create () in
  let n = Atomic.make 100 in
  let start = Unix.gettimeofday () in
  Fiber.spawn ~forbid:false children
    (List.init (Atomic.get n) @@ fun _ () ->
      Unix.sleepf 0.01;
      if 1 = Atomic.fetch_and_add n (-1) then
        Computation.finish children);
  Computation.await children;
  (* This is non-deterministic and might need to be changed if flaky *)
  Unix.gettimeofday () -. start < 1.0
- : bool = true
```
