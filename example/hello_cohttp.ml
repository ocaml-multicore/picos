open Picos_io
open Picos_io_cohttp
open Picos_std_finally
open Picos_std_structured

module String_ext = struct
  let drop_prefix_opt ~prefix s =
    if String.starts_with ~prefix s then
      let i = String.length prefix in
      Some (String.sub s i (String.length s - i))
    else None
end

module Option_ext = struct
  let ( >>= ) = Option.bind
  let ( >>- ) xO xy = Option.map xy xO

  let ( <|> ) xyO1 xyO2 x =
    match xyO1 x with Some _ as some -> some | None -> xyO2 x

  let filter p x = if p x then Some x else None
end

module Scheduler = struct
  open Option_ext

  let parse =
    let parse_0 name con s =
      String_ext.drop_prefix_opt ~prefix:name s
      >>- String.trim
      >>= filter (( = ) "")
      >>- fun _ -> con
    and parse_1 name con s =
      String_ext.drop_prefix_opt ~prefix:name s
      >>- String.trim >>= int_of_string_opt
      >>= filter (fun n -> 1 <= n && n <= Domain.recommended_domain_count ())
      >>- con
    in
    fun s ->
      match
        String.trim s
        |> (parse_0 "fifo" `Fifo <|> parse_0 "thread" `Thread
           <|> parse_1 "multififo" (fun n -> `Multififo n)
           <|> parse_1 "random" (fun n -> `Random n))
      with
      | None -> failwith "Unknown or unacceptable scheduler"
      | Some s -> s
end

let main ~port ~n_connections ~n_servers () =
  let@ server_socket =
    finally Unix.close @@ fun () ->
    Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
  in
  Unix.set_nonblock server_socket;
  Unix.bind server_socket Unix.(ADDR_INET (inet_addr_loopback, port));
  Unix.listen server_socket n_connections;
  let callback _conn _req _req_body =
    let res_body = "Hello world!\n" in
    Server.respond_string ~status:`OK ~body:res_body ()
  in
  Flock.join_after @@ fun () ->
  for _ = 1 to n_servers do
    Flock.fork @@ fun () -> Server.run (Server.make ~callback ()) server_socket
  done

let () =
  let port = ref 8082
  and n_connections = ref 300
  and scheduler = ref `Fifo
  and n_servers = ref 1 in
  let specs =
    [
      ("-port", Arg.Set_int port, "\t  Port");
      ("-conns", Arg.Set_int n_connections, "\t  Connections");
      ("-servers", Arg.Set_int n_servers, "\t  Server fibers");
      ( "-scheduler",
        Arg.String (fun s -> scheduler := Scheduler.parse s),
        "\t  Scheduler ('fifo' | 'thread' | 'multififo n' | 'random n')" );
    ]
  in
  Arg.parse specs ignore "";
  let main =
    main ~port:!port ~n_connections:!n_connections ~n_servers:!n_servers
  in
  match !scheduler with
  | `Fifo ->
      Printf.printf "Fifo\n%!";
      Picos_mux_fifo.run main
  | `Thread ->
      Printf.printf "Thread\n%!";
      Picos_mux_thread.run main
  | `Multififo n_domains ->
      Printf.printf "Multififo %d\n%!" n_domains;
      Picos_mux_multififo.run_on ~n_domains main
  | `Random n_domains ->
      Printf.printf "Random %d\n%!" n_domains;
      Picos_mux_random.run_on ~n_domains main
