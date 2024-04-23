open Foundation.Finally
open Elements
open Picos_stdio

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version
let use_nonblock = Sys.win32 || Random.bool (Random.self_init ())

let set_nonblock fd =
  if use_nonblock then
    try Unix.set_nonblock fd
    with _exn -> Printf.printf "  (Failed to set_nonblock)\n%!"

let is_opam_ci =
  match Sys.getenv "OPAM_REPO_CI" with
  | _ -> true
  | exception Not_found -> false

let main () =
  Bundle.run @@ fun bundle ->
  let n = 100 in
  let server_addr = ref None in

  let server =
    Bundle.fork bundle @@ fun () ->
    Printf.printf "  Server running\n%!";
    let@ client =
      finally Unix.close @@ fun () ->
      let@ socket =
        finally Unix.close @@ fun () ->
        Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
      in
      set_nonblock socket;
      match Unix.bind socket Unix.(ADDR_INET (inet_addr_loopback, 0)) with
      | () ->
          server_addr := Some (Unix.getsockname socket);
          Unix.listen socket 1;
          Printf.printf "  Server listening\n%!";
          Unix.accept ~cloexec:true socket |> fst
      | exception Unix.Unix_error (EPERM, _, _) when is_opam_ci -> raise Exit
    in
    set_nonblock client;
    let bytes = Bytes.create n in
    let n = Unix.read client bytes 0 (Bytes.length bytes) in
    Printf.printf "  Server read %d\n%!" n;
    let n = Unix.write client bytes 0 (n / 2) in
    Printf.printf "  Server wrote %d\n%!" n
  in

  let client =
    Bundle.fork bundle @@ fun () ->
    Printf.printf "  Client running\n%!";
    let@ socket =
      finally Unix.close @@ fun () ->
      Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
    in
    set_nonblock socket;
    let server_addr =
      let rec loop retries =
        match !server_addr with
        | None ->
            if retries < 0 then
              if is_opam_ci then raise Exit else failwith "No server address";
            Unix.sleepf 0.01;
            loop (retries - 1)
        | Some addr -> addr
      in
      loop 100
    in
    Unix.connect socket server_addr;
    Printf.printf "  Client connected\n%!";
    let bytes = Bytes.create n in
    let n = Unix.write socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client wrote %d\n%!" n;
    let n = Unix.read socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client read %d\n%!" n
  in

  Promise.await (Promise.both server client);

  Printf.printf "Server and Client test: OK\n%!"

let () =
  Printf.printf "Using %sblocking sockets and %s:\n%!"
    (if use_nonblock then "non-" else "")
    (if is_ocaml4 then "threads on OCaml 4" else "fibers on OCaml 5");
  try Test_scheduler.run main
  with Exit -> Printf.printf "Server and Client test: SKIPPED\n%!"
