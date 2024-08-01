open Picos_finally
open Picos_structured
open Picos_stdio
open Picos_sync

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version
let use_nonblock = Sys.win32 || Random.bool ()

let set_nonblock fd =
  if use_nonblock then
    try Unix.set_nonblock fd
    with _exn -> Printf.printf "  (Failed to set_nonblock)\n%!"

let is_opam_ci =
  match Sys.getenv "OPAM_REPO_CI" with
  | _ -> true
  | exception Not_found -> false

let main () =
  let n = 100 in

  let loopback_0 = Unix.(ADDR_INET (inet_addr_loopback, 0)) in
  let server_addr = ref loopback_0 in
  let mutex = Mutex.create () in
  let condition = Condition.create () in

  let server_looping () =
    Printf.printf "  Looping server running\n%!";
    let@ socket =
      finally Unix.close @@ fun () ->
      Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
    in
    set_nonblock socket;
    match Unix.bind socket loopback_0 with
    | () ->
        Mutex.protect mutex (fun () -> server_addr := Unix.getsockname socket);
        Condition.signal condition;
        Unix.listen socket 8;
        Printf.printf "  Server listening\n%!";
        Bundle.join_after @@ fun bundle ->
        while true do
          let^ client =
            finally Unix.close @@ fun () ->
            Printf.printf "  Server accepting\n%!";
            Unix.accept ~cloexec:true socket |> fst
          in
          Printf.printf "  Server accepted client\n%!";

          Bundle.fork bundle @@ fun () ->
          let@ client = move client in
          set_nonblock client;
          let bytes = Bytes.create n in
          let n = Unix.read client bytes 0 (Bytes.length bytes) in
          Printf.printf "  Server read %d\n%!" n;
          let n = Unix.write client bytes 0 (n / 2) in
          Printf.printf "  Server wrote %d\n%!" n
        done
    | exception Unix.Unix_error (EPERM, _, _) when is_opam_ci -> raise Exit
  in

  let server_recursive () =
    Printf.printf "  Recursive server running\n%!";
    let@ socket =
      finally Unix.close @@ fun () ->
      Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
    in
    set_nonblock socket;
    match Unix.bind socket Unix.(ADDR_INET (inet_addr_loopback, 0)) with
    | () ->
        Mutex.protect mutex (fun () -> server_addr := Unix.getsockname socket);
        Condition.signal condition;
        Unix.listen socket 8;
        Printf.printf "  Server listening\n%!";
        Bundle.join_after @@ fun bundle ->
        let rec accept () =
          let@ client =
            finally Unix.close @@ fun () ->
            Printf.printf "  Server accepting\n%!";
            Unix.accept ~cloexec:true socket |> fst
          in
          Printf.printf "  Server accepted client\n%!";
          Bundle.fork bundle accept;
          set_nonblock client;
          let bytes = Bytes.create n in
          let n = Unix.read client bytes 0 (Bytes.length bytes) in
          Printf.printf "  Server read %d\n%!" n;
          let n = Unix.write client bytes 0 (n / 2) in
          Printf.printf "  Server wrote %d\n%!" n
        in
        Bundle.fork bundle accept
    | exception Unix.Unix_error (EPERM, _, _) when is_opam_ci -> raise Exit
  in

  let server = if Random.bool () then server_looping else server_recursive in

  let client id () =
    Printf.printf "  Client %s running\n%!" id;
    let@ socket =
      finally Unix.close @@ fun () ->
      Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
    in
    set_nonblock socket;
    Unix.connect socket !server_addr;
    Printf.printf "  Client %s connected\n%!" id;
    let bytes = Bytes.create n in
    let n = Unix.write socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client %s wrote %d\n%!" id n;
    let n = Unix.read socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client %s read %d\n%!" id n
  in

  begin
    Bundle.join_after @@ fun bundle ->
    Bundle.fork bundle server;
    begin
      Mutex.protect mutex @@ fun () ->
      while !server_addr == loopback_0 do
        Condition.wait condition mutex
      done
    end;
    Run.all [ client "A"; client "B" ];
    Bundle.terminate bundle
  end;

  Printf.printf "Server and Client test: OK\n%!"

let () =
  Printf.printf "Using %sblocking sockets and %s:\n%!"
    (if use_nonblock then "non-" else "")
    (if is_ocaml4 then "threads on OCaml 4" else "fibers on OCaml 5");
  try Test_scheduler.run ~max_domains:4 main with
  | Exit -> Printf.printf "Server and Client test: SKIPPED\n%!"
  | exn -> Printf.printf "Error: %s\n%!" (Printexc.to_string exn)
