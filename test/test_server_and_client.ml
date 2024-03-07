open Foundation.Finally
open Elements

let main () =
  Bundle.run @@ fun bundle ->
  let n = 100 in
  let server_addr = ref None in

  let server =
    Bundle.fork bundle @@ fun () ->
    Printf.printf "  Server running\n%!";
    let@ client =
      finally Async_unix.close @@ fun () ->
      let@ socket =
        finally Async_unix.close @@ fun () ->
        Async_unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
      in
      Async_unix.set_nonblock socket;
      Async_unix.bind socket Async_unix.(ADDR_INET (inet_addr_loopback, 0));
      server_addr := Some (Async_unix.getsockname socket);
      Async_unix.listen socket 1;
      Printf.printf "  Server listening\n%!";
      Async_unix.accept ~cloexec:true socket |> fst
    in
    Async_unix.set_nonblock client;
    let bytes = Bytes.create n in
    let n = Async_unix.read client bytes 0 (Bytes.length bytes) in
    Printf.printf "  Server read %d\n%!" n;
    let n = Async_unix.write client bytes 0 (n / 2) in
    Printf.printf "  Server wrote %d\n%!" n
  in

  let client =
    Bundle.fork bundle @@ fun () ->
    Printf.printf "  Client running\n%!";
    let@ socket =
      finally Async_unix.close @@ fun () ->
      Async_unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
    in
    let server_addr =
      let rec loop retries =
        match !server_addr with
        | None ->
            if retries < 0 then failwith "No server address";
            Sleep.sleepf 0.01;
            loop (retries - 1)
        | Some addr -> addr
      in
      loop 100
    in
    Async_unix.connect socket server_addr;
    Printf.printf "  Client connected\n%!";
    Async_unix.set_nonblock socket;
    let bytes = Bytes.create n in
    let n = Async_unix.write socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client wrote %d\n%!" n;
    let n = Async_unix.read socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client read %d\n%!" n
  in

  Promise.await (Promise.both server client);

  Printf.printf "Server and Client test: OK\n%!"

let () =
  Printf.printf "Using fibers on OCaml 5 and threads on OCaml 4:\n%!";
  Test_scheduler.run main
