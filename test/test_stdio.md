# `Picos_stdio`

```ocaml
open Picos
open Picos_structured.Finally
open Picos_structured
open Picos_sync
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
  Bundle.join_after @@ fun bundle ->
  Bundle.fork bundle begin fun () ->
    let _ = Unix.select [] [] [] (-1.0) in
    Printf.printf "Impossible\n%!"
  end;
  Unix.sleepf 0.01;
  Bundle.terminate bundle
- : unit = ()
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

  Bundle.join_after @@ fun bundle ->
  Bundle.fork bundle begin fun () ->
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
  end;

  assert (1 = Unix.write_substring msg_out1 "!" 0 1);
  assert (1 = Unix.write_substring msg_out2 "!" 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

  assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

  Bundle.terminate bundle
Inn1
Inn2
Timeout
- : unit = ()
```
