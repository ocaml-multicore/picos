open Picos_structured
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

(** [Picos_lwt] gives us a direct-style [await] operation. *)
let await = Picos_lwt.await

let main () =
  Flock.join_after @@ fun () ->
  (* We use the [Picos_structured] library for structured concurrency. *)

  (* First we start the server. *)
  let server =
    Flock.fork_as_promise @@ fun () ->
    let callback _conn req body =
      let uri = req |> Request.uri |> Uri.to_string in
      let meth = req |> Request.meth |> Code.string_of_method in
      let headers = req |> Request.headers |> Header.to_string in
      let body = body |> Body.to_string |> await in
      let body =
        Printf.sprintf "Uri: %s\nMethod: %s\n\n%sBody: %s" uri meth headers body
      in
      Server.respond_string ~status:`OK ~body ()
    in
    let ctx = Conduit_lwt_unix.init ~src:"127.0.0.1" () |> await in
    let ctx = Client.custom_ctx ~ctx () in
    Server.create ~ctx ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
    |> await
  in

  (* Then we GET a response from the server. *)
  let _resp, body =
    await (Client.get (Uri.of_string "http://127.0.0.1:8000/hello-lwt"))
  in
  Printf.printf "%s\n%!" (await (Body.to_string body));

  (* Finally we terminate the server. *)
  Promise.terminate server

let () = Lwt_main.run @@ Picos_lwt_unix.run main
