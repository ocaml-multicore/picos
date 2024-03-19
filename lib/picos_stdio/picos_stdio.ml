type fd = { rc : Unix.file_descr Picos_rc.t; mutable nonblock : bool }

module In_channel = struct
  type t = fd
end

module Out_channel = struct
  type t = fd
end

module Unix = struct
  include Unix

  type file_descr = fd

  let get_nonblock fd = fd.nonblock

  let unsafe_getfd fd =
    if Picos_rc.has_been_disposed fd.rc then
      raise (Unix.Unix_error (EBADF, "", ""))
    else Picos_rc.unsafe_get fd.rc

  let rcn fd =
    let rc = Picos_rc.make ~dispose:Unix.close fd in
    let nonblock =
      match Unix.set_nonblock fd with
      | () -> true
      | exception Unix.Unix_error (Unix.ENOTSOCK, _, _) when Sys.win32 -> false
    in
    { rc; nonblock }

  let stdin = rcn Unix.stdin
  and stdout = rcn Unix.stdout
  and stderr = rcn Unix.stderr

  let openfile path flags file_perm = rcn (Unix.openfile path flags file_perm)
  let close fd = Picos_rc.dispose fd.rc

  let rec fsync fd =
    try Unix.fsync (unsafe_getfd fd) with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `W;
        fsync fd
    | Unix.Unix_error (EINTR, _, _) -> fsync fd

  let rec read fd bytes pos len =
    try Unix.read (unsafe_getfd fd) bytes pos len with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `R;
        read fd bytes pos len
    | Unix.Unix_error (EINTR, _, _) -> read fd bytes pos len

  let rec write fd bytes pos len =
    try Unix.write (unsafe_getfd fd) bytes pos len with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `W;
        write fd bytes pos len
    | Unix.Unix_error (EINTR, _, _) -> write fd bytes pos len

  let rec single_write fd bytes pos len =
    try Unix.single_write (unsafe_getfd fd) bytes pos len with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `W;
        single_write fd bytes pos len
    | Unix.Unix_error (EINTR, _, _) -> single_write fd bytes pos len

  let rec write_substring fd string pos len =
    try Unix.write_substring (unsafe_getfd fd) string pos len with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `W;
        write_substring fd string pos len
    | Unix.Unix_error (EINTR, _, _) -> write_substring fd string pos len

  let rec single_write_substring fd string pos len =
    try Unix.single_write_substring (unsafe_getfd fd) string pos len with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `W;
        single_write_substring fd string pos len
    | Unix.Unix_error (EINTR, _, _) -> single_write_substring fd string pos len

  (* *)

  let in_channel_of_descr _ = failwith "TODO"
  let out_channel_of_descr _ = failwith "TODO"
  let descr_of_in_channel _ = failwith "TODO"
  let descr_of_out_channel _ = failwith "TODO"

  (* *)

  let lseek fd amount seek_command =
    Unix.lseek (unsafe_getfd fd) amount seek_command

  let ftruncate fd size = Unix.ftruncate (unsafe_getfd fd) size

  (* *)

  let fstat fd = Unix.fstat (unsafe_getfd fd)
  let isatty fd = Unix.isatty (unsafe_getfd fd)

  (* *)

  module LargeFile = struct
    include Unix.LargeFile

    let lseek fd amount seek_command =
      Unix.LargeFile.lseek (unsafe_getfd fd) amount seek_command

    let ftruncate fd size = Unix.LargeFile.ftruncate (unsafe_getfd fd) size
    let fstat fd = Unix.LargeFile.fstat (unsafe_getfd fd)
  end

  (* *)

  let map_file fd ?pos kind layout shared dims =
    Unix.map_file (unsafe_getfd fd) ?pos kind layout shared dims

  (* *)

  let fchmod fd file_perm = Unix.fchmod (unsafe_getfd fd) file_perm
  let fchown fd uid gid = Unix.fchown (unsafe_getfd fd) uid gid

  (* *)

  let dup ?cloexec fd = rcn (Unix.dup ?cloexec (unsafe_getfd fd))

  let dup2 ?cloexec src dst =
    Unix.dup2 ?cloexec (unsafe_getfd src) (unsafe_getfd dst)

  let set_nonblock fd =
    Unix.set_nonblock (unsafe_getfd fd);
    fd.nonblock <- true

  let clear_nonblock fd =
    Unix.clear_nonblock (unsafe_getfd fd);
    fd.nonblock <- false

  let set_close_on_exec fd = Unix.set_close_on_exec (unsafe_getfd fd)
  let clear_close_on_exec fd = Unix.clear_close_on_exec (unsafe_getfd fd)

  (* *)

  let pipe ?cloexec () =
    let inn, out = Unix.pipe ?cloexec () in
    (rcn inn, rcn out)

  (* *)

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args (unsafe_getfd stdin) (unsafe_getfd stdout)
      (unsafe_getfd stderr)

  let create_process_env prog args env stdin stdout stderr =
    Unix.create_process_env prog args env (unsafe_getfd stdin)
      (unsafe_getfd stdout) (unsafe_getfd stderr)

  let open_process_in _ = failwith "TODO"
  let open_process_out _ = failwith "TODO"
  let open_process _ = failwith "TODO"
  let open_process_full _ = failwith "TODO"
  let open_process_args _ = failwith "TODO"
  let open_process_args_in _ = failwith "TODO"
  let open_process_args_out _ = failwith "TODO"
  let open_process_args_full _ = failwith "TODO"
  let process_in_pid _ = failwith "TODO"
  let process_out_pid _ = failwith "TODO"
  let process_pid _ = failwith "TODO"
  let process_full_pid _ = failwith "TODO"
  let close_process_in _ = failwith "TODO"
  let close_process_out _ = failwith "TODO"
  let close_process _ = failwith "TODO"
  let close_process_full _ = failwith "TODO"

  (* *)

  let select _ = failwith "TODO"

  (* *)

  let lockf fd lock_command length =
    Unix.lockf (unsafe_getfd fd) lock_command length

  (* *)

  let socket ?cloexec socket_domain socket_type protocol =
    rcn (Unix.socket ?cloexec socket_domain socket_type protocol)

  let socketpair ?cloexec socket_domain socket_type mystery =
    let fst, snd = Unix.socketpair ?cloexec socket_domain socket_type mystery in
    (rcn fst, rcn snd)

  let rec accept ?cloexec fd =
    try
      let fd, sockaddr = Unix.accept ?cloexec (unsafe_getfd fd) in
      (rcn fd, sockaddr)
    with
    | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) when fd.nonblock ->
        Picos_select.await_on fd.rc `R;
        accept ?cloexec fd
    | Unix.Unix_error (EINTR, _, _) -> accept ?cloexec fd

  let bind fd sockaddr = Unix.bind (unsafe_getfd fd) sockaddr

  let rec connect fd sockaddr =
    try Unix.connect (unsafe_getfd fd) sockaddr with
    | Unix.Unix_error ((EAGAIN | EINPROGRESS | EWOULDBLOCK), _, _)
      when fd.nonblock -> begin
        Picos_select.await_on fd.rc `W;
        match Unix.getsockopt_error (unsafe_getfd fd) with
        | None -> ()
        | Some error -> raise (Unix.Unix_error (error, "connect", ""))
      end
    | Unix.Unix_error (EINTR, _, _) -> connect fd sockaddr

  let listen fd max_pending = Unix.listen (unsafe_getfd fd) max_pending

  let shutdown fd shutdown_command =
    Unix.shutdown (unsafe_getfd fd) shutdown_command

  let getsockname fd = Unix.getsockname (unsafe_getfd fd)
  let getpeername fd = Unix.getpeername (unsafe_getfd fd)

  let recv fd bytes offset length flags =
    Unix.recv (unsafe_getfd fd) bytes offset length flags

  let recvfrom fd bytes offset length flags =
    Unix.recvfrom (unsafe_getfd fd) bytes offset length flags

  let send fd bytes offset length flags =
    Unix.send (unsafe_getfd fd) bytes offset length flags

  let send_substring fd string offset length flags =
    Unix.send_substring (unsafe_getfd fd) string offset length flags

  let sendto fd bytes offset length flags sockaddr =
    Unix.sendto (unsafe_getfd fd) bytes offset length flags sockaddr

  let sendto_substring fd string offset length flags sockaddr =
    Unix.sendto_substring (unsafe_getfd fd) string offset length flags sockaddr

  (* *)

  let getsockopt fd option = Unix.getsockopt (unsafe_getfd fd) option
  let setsockopt fd option bool = Unix.setsockopt (unsafe_getfd fd) option bool
  let getsockopt_int fd option = Unix.getsockopt_int (unsafe_getfd fd) option

  let setsockopt_int fd option int =
    Unix.setsockopt_int (unsafe_getfd fd) option int

  let getsockopt_optint fd option =
    Unix.getsockopt_optint (unsafe_getfd fd) option

  let setsockopt_optint fd option optint =
    Unix.setsockopt_optint (unsafe_getfd fd) option optint

  let getsockopt_float fd option =
    Unix.getsockopt_float (unsafe_getfd fd) option

  let setsockopt_float fd option float =
    Unix.setsockopt_float (unsafe_getfd fd) option float

  let getsockopt_error fd = Unix.getsockopt_error (unsafe_getfd fd)

  (* *)

  let open_connection _ = failwith "TODO"
  let shutdown_connection _ = failwith "TODO"
  let establish_server _ = failwith "TODO"

  (* *)

  let tcgetattr fd = Unix.tcgetattr (unsafe_getfd fd)

  let tcsetattr fd setattr_when terminal_io =
    Unix.tcsetattr (unsafe_getfd fd) setattr_when terminal_io

  let tcsendbreak fd duration = Unix.tcsendbreak (unsafe_getfd fd) duration
  let tcdrain fd = Unix.tcdrain (unsafe_getfd fd)
  let tcflush fd flush_queue = Unix.tcflush (unsafe_getfd fd) flush_queue
  let tcflow fd flow_action = Unix.tcflow (unsafe_getfd fd) flow_action
end
