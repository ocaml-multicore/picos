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

```ocaml
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

```ocaml
# Test_scheduler.run @@ fun () ->
  let start = Unix.gettimeofday () in
  let _ = Unix.select [] [] [] 0.1 in
  let d = Unix.gettimeofday () -. start in
  (* This is non-deterministic and might need to be changed if flaky *)
  0.1 <= d && d <= 1.0
- : bool = true
```

```ocaml
# Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  let exited = Trigger.create () in
  Fiber.spawn ~forbid:false computation [ fun () ->
    match Unix.select [] [] [] (-1.0) with
    | _ -> Printf.printf "Impossible\n%!"
    | exception Exit ->
      Trigger.signal exited ];
  Unix.sleepf 0.01;
  assert (Trigger.is_initial exited);
  Computation.cancel computation (Exn_bt.get_callstack 0 Exit);
  Trigger.await exited
- : Picos_exn_bt.t option = None
```

```ocaml
# Test_scheduler.run @@ fun () ->

  let@ msg_inn1, msg_out1 =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in
  let@ msg_inn2, msg_out2 =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in
  let@ syn_inn, syn_out =
    finally Unix.close_pair @@ fun () ->
    Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
  in

  Unix.set_nonblock msg_inn1;
  Unix.set_nonblock msg_out1;
  Unix.set_nonblock msg_inn2;
  Unix.set_nonblock msg_out2;
  Unix.set_nonblock syn_inn;
  Unix.set_nonblock syn_out;

  let consumer = Computation.create () in
  let finished = Trigger.create () in
  Fiber.spawn ~forbid:false consumer [ fun () ->
    try
      while true do
        match Unix.select [ msg_inn1; msg_inn2 ] [] [] 0.1 with
        | inns, _, _ ->
          if List.exists ((==) msg_inn1) inns then begin
            Printf.printf "Inn1\n%!";
            assert (1 = Unix.read msg_inn1 (Bytes.create 1) 0 1);
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end;
          if List.exists ((==) msg_inn2) inns then begin
            Printf.printf "Inn2\n%!";
            assert (1 = Unix.read msg_inn2 (Bytes.create 1) 0 1);
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end;
          if [] == inns then begin
            Printf.printf "Timeout\n%!";
            assert (1 = Unix.write_substring syn_out "!" 0 1)
          end
      done
    with Exit -> Trigger.signal finished  ];

  assert (1 = Unix.write_substring msg_out1 "!" 0 1);
  assert (1 = Unix.write_substring msg_out2 "!" 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

  Computation.cancel consumer (Exn_bt.get_callstack 0 Exit);
  Trigger.await finished
Inn1
Inn2
Timeout
- : Picos_exn_bt.t option = None
```
