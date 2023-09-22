open Picos
open Finally

let () =
  Fifos.run ~forbid:false @@ fun () ->
  Bundle.run @@ fun bundle ->
  let n = 100 in
  let port =
    Random.self_init ();
    Random.int 1000 + 3000
  in
  let server_addr =
    Async_unix.ADDR_INET (Async_unix.inet_addr_loopback, port)
  in

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
      Async_unix.bind socket server_addr;
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
    Fiber.yield ();
    Async_unix.connect socket server_addr;
    Printf.printf "  Client connected\n%!";
    let bytes = Bytes.create n in
    let n = Async_unix.write socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client wrote %d\n%!" n;
    let n = Async_unix.read socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client read %d\n%!" n
  in

  Promise.await (Promise.both server client);

  Printf.printf "Server and Client test: OK\n%!"
