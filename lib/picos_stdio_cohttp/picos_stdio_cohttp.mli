(** Minimalistic {{:https://github.com/mirage/ocaml-cohttp/} Cohttp}
    implementation using {!Picos_stdio} for {!Picos}.

    ⚠️ This library is currently minimalistic and experimental and is highly
    likely to change.  Feedback from potential users is welcome! *)

open Picos_stdio

(** {1 Modules} *)

(** Convenience functions for constructing requests and processing responses.

    Please consult the
    {{:https://ocaml.org/p/cohttp/latest/doc/Cohttp/Generic/Client/module-type-S/index.html} CoHTTP documentation}. *)
module Client : sig
  include
    Cohttp.Generic.Client.S
      with type 'a io = 'a
      with type 'a with_context = 'a
      with type body = Cohttp.Body.t
end

(** Convenience functions for processing requests and constructing responses.

    Please consult the
    {{:https://ocaml.org/p/cohttp/latest/doc/Cohttp/Generic/Server/module-type-S/index.html} CoHTTP documentation}. *)
module Server : sig
  include
    Cohttp.Generic.Server.S
      with type 'a IO.t = 'a
      with type IO.conn = Unix.file_descr
      with type body = Cohttp.Body.t

  val run : t -> IO.conn -> unit
  (** [run server socket] starts running a server that {{!Unix.accept} accepts}
      clients on the specified [socket].  This never returns normally. *)
end

(** {1 Examples}

    First we open some modules for convenience:

    {[
      open Cohttp
      open Picos_std_finally
      open Picos_std_structured
      open Picos_stdio
      open Picos_stdio_cohttp
    ]}

    {2 A server and client}

    Let's build a simple hello server.  We first define a function that creates
    and configures a socket for the server:

    {[
      let server_create ?(max_pending_reqs = 8) addr =
        let socket =
          Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
        in
        match
          Unix.set_nonblock socket;
          Unix.bind socket addr;
          Unix.listen socket max_pending_reqs
        with
        | () -> socket
        | exception exn ->
          Unix.close socket;
          raise exn
    ]}

    The reason for doing it like this, as we'll see later, is that we want the
    OS to decide the port for our server.  Also note that we explicitly set the
    socket to non-blocking mode, which is what we should do with {!Picos_stdio}
    whenever possible.

    Then we'll define a function that runs a server given a socket:

    {[
      let server_run socket =
        let callback _conn _req body =
          let body =
            Printf.sprintf "Hello, %s!"
              (Body.to_string body)
          in
          Server.respond_string ~status:`OK ~body ()
        in
        Server.run (Server.make ~callback ()) socket
    ]}

    The idea is that the body of the request is the name to be greeted
    in the body of the response.

    A client then posts to the specified uri and returns the response body:

    {[
      let client uri name =
        let resp, body =
          Client.post ~body:(`String name) uri
        in
        if Response.status resp != `OK then
          failwith "Not OK";
        Body.to_string body
    ]}

    Now we are ready to put everything together:

    {[
      # Picos_mux_random.run_on ~n_domains:2 @@ fun () ->

        let@ server_socket =
          finally Unix.close @@ fun () ->
          server_create
            Unix.(ADDR_INET (inet_addr_loopback, 0))
        in

        let server_port =
          match Unix.getsockname server_socket with
          | ADDR_UNIX _ -> failwith "impossible"
          | ADDR_INET (_, port) -> port
        in

        let server_uri =
          Uri.make
            ~scheme:"http"
            ~host:"127.0.0.1"
            ~port:server_port
            ()
        in

        Flock.join_after ~on_return:`Terminate @@ fun () ->

        Flock.fork begin fun () ->
          server_run server_socket
        end;

        client server_uri "World"
      - : string = "Hello, World!"
    ]}

    We first create the [server_socket] and obtain the [server_port] and
    ultimately the [server_uri] from it — typically one can avoid this
    complexity and use a fixed port.  We then create a
    {{!Picos_std_structured.Flock} flock} for running the server as a concurrent
    fiber, which we arrange to terminate at the end of the scope.  Finally we
    act as the client to get a greeting from the server. *)
