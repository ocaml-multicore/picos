open Picos_finally
open Picos_stdio
open Picos_structured

(* *)

module Private = struct
  module IO = struct
    let[@inline never] locked () = invalid_arg "locked"

    type 'a t = 'a

    let ( >>= ) v f = f v
    let return v = v

    type conn = Unix.file_descr

    type ic = {
      mutable bytes : Bytes.t;
      mutable start : int;
      mutable stop : int;
      mutable locked : int;
      file_descr : Unix.file_descr;
    }

    let min_buffer = 4096

    let ic file_descr =
      let bytes = Bytes.create min_buffer in
      { bytes; start = 0; stop = 0; locked = 0; file_descr }

    let rec refill ic =
      if
        min_buffer < Bytes.length ic.bytes
        && (ic.stop - ic.start) * 4 < Bytes.length ic.bytes
      then begin
        let bytes = Bytes.create (Bytes.length ic.bytes lsr 1) in
        Bytes.blit ic.bytes ic.start bytes 0 (ic.stop - ic.start);
        ic.bytes <- bytes;
        ic.stop <- ic.stop - ic.start;
        ic.start <- 0
      end;
      if ic.stop < Bytes.length ic.bytes then begin
        let n =
          Unix.read ic.file_descr ic.bytes ic.stop
            (Bytes.length ic.bytes - ic.stop)
        in
        if 0 < n then begin
          ic.stop <- ic.stop + n;
          `Ok
        end
        else `Eof
      end
      else if 0 < ic.start then begin
        if 0 < ic.locked then locked ();
        let stop = ic.stop - ic.start in
        Bytes.blit ic.bytes ic.start ic.bytes 0 stop;
        ic.start <- 0;
        ic.stop <- stop;
        refill ic
      end
      else begin
        let bytes = Bytes.create (Bytes.length ic.bytes * 2) in
        Bytes.blit ic.bytes 0 bytes 0 ic.stop;
        ic.bytes <- bytes;
        refill ic
      end

    let with_input_buffer ic ~f =
      let string = Bytes.unsafe_to_string ic.bytes in
      ic.locked <- ic.locked + 1;
      match f string ~pos:ic.start ~len:(ic.stop - ic.start) with
      | result, n ->
          ic.locked <- ic.locked - 1;
          ic.start <- ic.start + n;
          result
      | exception exn ->
          ic.locked <- ic.locked - 1;
          raise exn

    let read_line_finish ic i =
      if i = 0 && ic.start = ic.stop then None
      else
        let n =
          i - Bool.to_int (0 < i && Bytes.get ic.bytes (ic.start + i - 1) = '\r')
        in
        let result = Some (Bytes.sub_string ic.bytes ic.start n) in
        ic.start <- ic.start + i + Bool.to_int (ic.start + i < ic.stop - 1);
        result

    let rec read_line_to_lf ic i =
      if ic.start + i < ic.stop then
        if Bytes.get ic.bytes (ic.start + i) <> '\n' then
          read_line_to_lf ic (i + 1)
        else read_line_finish ic i
      else
        match refill ic with
        | `Ok -> read_line_to_lf ic i
        | `Eof -> read_line_finish ic i

    let read_line ic = read_line_to_lf ic 0

    let rec read ic max_chars =
      let available = ic.stop - ic.start in
      if 0 < available then begin
        let n = Int.min available max_chars in
        let result = Bytes.sub_string ic.bytes ic.start n in
        ic.start <- ic.start + n;
        result
      end
      else match refill ic with `Ok -> read ic max_chars | `Eof -> ""

    type oc = Unix.file_descr

    let write oc string =
      let n = Unix.write_substring oc string 0 (String.length string) in
      assert (n = String.length string)

    let flush = Unix.fsync
  end
end

module Request = Cohttp.Request.Private.Make (Private.IO)
module Response = Cohttp.Response.Private.Make (Private.IO)

module Body = struct
  let rec read_with reader read_body_chunk body =
    match read_body_chunk reader with
    | Cohttp.Transfer.Done -> raise End_of_file
    | Chunk data ->
        Buffer.add_string body data;
        read_with reader read_body_chunk body
    | Final_chunk data ->
        Buffer.add_string body data;
        `String (Buffer.contents body)

  let read_with has_body make_body_reader read_body_chunk source input =
    match has_body source with
    | `No | `Unknown -> `Empty
    | `Yes ->
        let body = Buffer.create 4096 in
        let reader = make_body_reader source input in
        read_with reader read_body_chunk body

  let write_with write_body = function
    | `Empty -> ignore
    | `String body -> fun writer -> write_body writer body
    | `Strings bodies -> fun writer -> List.iter (write_body writer) bodies
end

module Client =
  Cohttp.Generic.Client.Make
    (struct
      type 'a io = 'a
      type body = Cohttp.Body.t
      type 'a with_context = 'a

      let map_context with_context fn = fn with_context

      let resolve uri =
        match Uri.scheme uri with
        | Some "http" -> begin
            let host = Uri.host_with_default ~default:"localhost" uri in
            let service =
              match Uri.port uri with
              | Some port -> Int.to_string port
              | None -> Uri.scheme uri |> Option.value ~default:"http"
            in
            try
              Unix.getaddrinfo host service []
              |> List.find @@ fun ai -> ai.Unix.ai_socktype == SOCK_STREAM
            with Not_found -> failwith "failed to resolve hostname"
          end
        | x ->
            Fmt.failwith "Unknown scheme %a"
              Fmt.(option ~none:(any "None") Dump.string)
              x

      let call ?headers ?body ?(chunked = false) meth uri =
        let ai = resolve uri in
        let@ socket =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true ai.ai_family ai.ai_socktype 0
        in
        Unix.set_nonblock socket;
        Unix.connect socket ai.ai_addr;
        let body_length =
          if chunked then None
          else
            match body with
            | None -> Some 0L
            | Some body -> Some (Cohttp.Body.length body)
        in
        let request =
          Cohttp.Request.make_for_client ?headers
            ~chunked:(Option.is_none body_length)
            ?body_length meth uri
        in
        Request.write ~flush:false
          (match body with
          | None -> ignore
          | Some body -> Body.write_with Request.write_body body)
          request socket;
        Unix.shutdown socket SHUTDOWN_SEND;
        let input = Private.IO.ic socket in
        match Response.read input with
        | `Eof -> failwith "connection closed by peer"
        | `Invalid reason -> failwith reason
        | `Ok response ->
            let body =
              Body.read_with Cohttp.Response.has_body Response.make_body_reader
                Response.read_body_chunk response input
            in
            (response, body)
    end)
    (Private.IO)

module Server = struct
  module IO = Private.IO

  type body = Cohttp.Body.t
  type conn = IO.conn * Cohttp.Connection.t [@@warning "-3"]
  type response = Http.Response.t * body

  type response_action =
    [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit IO.t)
    | `Response of response ]

  type t = {
    callback : conn -> Http.Request.t -> body -> response_action IO.t;
    conn_closed : conn -> unit;
  }

  let make_response_action ?(conn_closed = ignore) ~callback () =
    { callback; conn_closed }

  let make_expert ?conn_closed ~callback () =
    let callback conn req body = `Expert (callback conn req body) in
    make_response_action ?conn_closed ~callback ()

  let make ?conn_closed ~callback () =
    let callback conn req body = `Response (callback conn req body) in
    make_response_action ?conn_closed ~callback ()

  let respond ?headers ?flush ~status ~body () =
    let encoding =
      match headers with
      | None -> Cohttp.Body.transfer_encoding body
      | Some headers -> (
          match Cohttp.Header.get_transfer_encoding headers with
          | Http.Transfer.Unknown -> Cohttp.Body.transfer_encoding body
          | t -> t)
    in
    let res = Cohttp.Response.make ~status ?flush ~encoding ?headers () in
    (res, body)

  let respond_string ?headers ?flush ~status ~body () =
    let res =
      Cohttp.Response.make ~status ?flush
        ~encoding:(Http.Transfer.Fixed (Int64.of_int (String.length body)))
        ?headers ()
    in
    (res, `String body)

  let rec handle t conn ic oc =
    match Request.read ic with
    | `Eof -> t.conn_closed conn
    | `Invalid _data -> t.conn_closed conn
    | `Ok req -> begin
        let body =
          Body.read_with Http.Request.has_body Request.make_body_reader
            Request.read_body_chunk req ic
        in
        match t.callback conn req body with
        | `Expert (res, io_handler) ->
            Response.write_header res oc;
            io_handler ic oc;
            handle t conn ic oc
        | `Response (res, body) ->
            let keep_alive =
              Http.Request.is_keep_alive req && Http.Response.is_keep_alive res
            in
            if not keep_alive then Unix.shutdown ic.file_descr SHUTDOWN_RECEIVE;
            let headers =
              Http.Header.add_unless_exists
                (Http.Response.headers res)
                "connection"
                (if keep_alive then "keep-alive" else "close")
            in
            let res = { res with headers } in
            Response.write (Body.write_with Response.write_body body) res oc;
            if keep_alive then handle t conn ic oc
      end

  let callback t io ic oc =
    let id = (Cohttp.Connection.create () [@ocaml.warning "-3"]) in
    let conn = (io, id) in
    try handle t conn ic oc
    with Unix.Unix_error (ECONNRESET, _, _) | Unix.Unix_error (EPIPE, _, _) ->
      t.conn_closed conn

  let run t server_socket =
    Bundle.join_after @@ fun bundle ->
    let rec accept () =
      let@ client_socket =
        finally Unix.close @@ fun () ->
        Unix.accept ~cloexec:true server_socket |> fst
      in
      Bundle.fork bundle accept;
      Unix.set_nonblock client_socket;
      callback t client_socket (Private.IO.ic client_socket) client_socket
    in
    accept ()
end
