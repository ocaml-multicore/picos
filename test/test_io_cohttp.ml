open Cohttp
open Picos_io
open Picos_io_cohttp
open Picos_std_finally
open Picos_std_structured

let is_opam_ci =
  match Sys.getenv "OPAM_REPO_CI" with
  | _ -> true
  | exception Not_found -> false

let main () =
  Printexc.record_backtrace true;

  (* First we create, bind, and start listening on the server socket. *)
  let@ server_socket =
    finally Unix.close @@ fun () ->
    Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
  in
  Unix.set_nonblock server_socket;
  match Unix.bind server_socket Unix.(ADDR_INET (inet_addr_loopback, 0)) with
  | exception Unix.Unix_error (EPERM, _, _) when is_opam_ci -> ()
  | () ->
      Unix.listen server_socket 8;
      (* We create a scope for fibers to be terminated on return. *)
      Flock.join_after ~on_return:`Terminate @@ fun () ->
      begin
        (* Then we start the server. *)
        Flock.fork @@ fun () ->
        let callback _conn req body =
          let uri = req |> Request.uri |> Uri.to_string in
          let meth = req |> Request.meth |> Code.string_of_method in
          let headers = req |> Request.headers |> Header.to_string in
          let body = body |> Body.to_string in
          let body =
            Printf.sprintf "Uri: %s\nMethod: %s\n\n%sBody: %s" uri meth headers
              body
          in
          Server.respond_string ~status:`OK ~body ()
        in
        Server.run (Server.make ~callback ()) server_socket
      end;
      let server_uri =
        match Unix.getsockname server_socket with
        | ADDR_UNIX _ -> failwith "impossible"
        | ADDR_INET (addr, port) ->
            Printf.sprintf "http://%s:%d/hello-io-cohttp"
              (Unix.string_of_inet_addr addr)
              port
      in
      (* Then we call the server. *)
      let _resp, body =
        Client.post ~body:(`String "It's-a-Me, Picos!")
          (Uri.of_string server_uri)
      in
      Printf.printf "%s\n%!" (Body.to_string body)

let () = Test_scheduler.run ~max_domains:3 main
