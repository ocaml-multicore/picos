module Unix = struct
  include Unix

  type file_descr = Picos_fd.t

  let is_oob flag = MSG_OOB == flag

  (* The retry wrappers below are written to avoid closure allocations. *)

  let progress_1 fd x1 fn op name =
    try fn (Picos_fd.unsafe_get fd) x1
    with
    (* The documentation of [bind] and [connect] does not mention [EAGAIN] or
       [EWOULDBLOCK], but on Windows we get those errors. *)
    | Unix.Unix_error ((EAGAIN | EINPROGRESS | EWOULDBLOCK), _, _) ->
      begin
        let fd = Picos_select.await_on fd op in
        match Unix.getsockopt_error (Picos_fd.unsafe_get fd) with
        | None -> ()
        | Some error -> raise (Unix.Unix_error (error, name, ""))
      end

  let rec again_0 fd fn op =
    try fn (Picos_fd.unsafe_get fd)
    with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      again_0 (Picos_select.await_on fd op) fn op

  let rec again_cloexec_0 ?cloexec fd fn op =
    try fn ?cloexec (Picos_fd.unsafe_get fd)
    with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      again_cloexec_0 ?cloexec (Picos_select.await_on fd op) fn op

  let rec again_3 fd x1 x2 x3 fn op =
    try fn (Picos_fd.unsafe_get fd) x1 x2 x3
    with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      again_3 (Picos_select.await_on fd op) x1 x2 x3 fn op

  let rec again_4 fd x1 x2 x3 x4 fn op =
    try fn (Picos_fd.unsafe_get fd) x1 x2 x3 x4
    with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      again_4 (Picos_select.await_on fd op) x1 x2 x3 x4 fn op

  let rec again_5 fd x1 x2 x3 x4 x5 fn op =
    try fn (Picos_fd.unsafe_get fd) x1 x2 x3 x4 x5
    with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      again_5 (Picos_select.await_on fd op) x1 x2 x3 x4 x5 fn op

  let rcn ?dispose fd =
    let fd = Picos_fd.create ?dispose fd in
    match Unix.set_nonblock (Picos_fd.unsafe_get fd) with
    | () -> fd
    | exception Unix.Unix_error (Unix.ENOTSOCK, _, _) when Sys.win32 ->
        (* TODO: Non-blocking is supported only on sockets on Windows. *)
        fd

  let stdin = rcn ~dispose:false Unix.stdin
  and stdout = rcn ~dispose:false Unix.stdout
  and stderr = rcn ~dispose:false Unix.stderr

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/open.html *)
  let openfile path flags file_perm = rcn (Unix.openfile path flags file_perm)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/close.html *)
  let close fd = Picos_fd.decr ~close:true fd

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fsync.html *)
  let fsync fd = again_0 fd Unix.fsync `W

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/read.html *)
  let read fd bytes pos len = again_3 fd bytes pos len Unix.read `R

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/write.html *)
  let write fd bytes pos len = again_3 fd bytes pos len Unix.write `W

  let single_write fd bytes pos len =
    again_3 fd bytes pos len Unix.single_write `W

  let write_substring fd string pos len =
    again_3 fd string pos len Unix.write_substring `W

  let single_write_substring fd string pos len =
    again_3 fd string pos len Unix.single_write_substring `W

  (* *)

  (*let in_channel_of_descr _ = failwith "TODO"*)
  (*let out_channel_of_descr _ = failwith "TODO"*)
  (*let descr_of_in_channel _ = failwith "TODO"*)
  (*let descr_of_out_channel _ = failwith "TODO"*)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/lseek.html *)
  let lseek fd amount seek_command =
    Unix.lseek (Picos_fd.unsafe_get fd) amount seek_command

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/ftruncate.html *)
  let ftruncate fd size = Unix.ftruncate (Picos_fd.unsafe_get fd) size

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fstat.html *)
  let fstat fd = Unix.fstat (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/isatty.html *)
  let isatty fd = Unix.isatty (Picos_fd.unsafe_get fd)

  (* *)

  module LargeFile = struct
    include Unix.LargeFile

    (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/lseek.html *)
    let lseek fd amount seek_command =
      Unix.LargeFile.lseek (Picos_fd.unsafe_get fd) amount seek_command

    (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/ftruncate.html *)
    let ftruncate fd size =
      Unix.LargeFile.ftruncate (Picos_fd.unsafe_get fd) size

    (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fstat.html *)
    let fstat fd = Unix.LargeFile.fstat (Picos_fd.unsafe_get fd)
  end

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/mmap.html

     This can raise EAGAIN, but it probably should not be handled? *)
  let map_file fd ?pos kind layout shared dims =
    Unix.map_file (Picos_fd.unsafe_get fd) ?pos kind layout shared dims

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fchmod.html *)
  let fchmod fd file_perm = Unix.fchmod (Picos_fd.unsafe_get fd) file_perm

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fchown.html *)
  let fchown fd uid gid = Unix.fchown (Picos_fd.unsafe_get fd) uid gid

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/dup.html *)
  let dup ?cloexec fd = rcn (Unix.dup ?cloexec (Picos_fd.unsafe_get fd))

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/dup.html *)
  let dup2 ?cloexec src dst =
    Unix.dup2 ?cloexec (Picos_fd.unsafe_get src) (Picos_fd.unsafe_get dst)

  (*let set_nonblock _ = failwith "TODO"*)
  (*let clear_nonblock _ = failwith "TODO"*)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fcntl.html *)
  let set_close_on_exec fd = Unix.set_close_on_exec (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fcntl.html *)
  let clear_close_on_exec fd = Unix.clear_close_on_exec (Picos_fd.unsafe_get fd)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/pipe.html *)
  let pipe ?cloexec () =
    let inn, out = Unix.pipe ?cloexec () in
    (rcn inn, rcn out)

  (* *)

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args
      (Picos_fd.unsafe_get stdin)
      (Picos_fd.unsafe_get stdout)
      (Picos_fd.unsafe_get stderr)

  let create_process_env prog args env stdin stdout stderr =
    Unix.create_process_env prog args env
      (Picos_fd.unsafe_get stdin)
      (Picos_fd.unsafe_get stdout)
      (Picos_fd.unsafe_get stderr)

  (*let open_process_in _ = failwith "TODO"*)
  (*let open_process_out _ = failwith "TODO"*)
  (*let open_process _ = failwith "TODO"*)
  (*let open_process_full _ = failwith "TODO"*)
  (*let open_process_args _ = failwith "TODO"*)
  (*let open_process_args_in _ = failwith "TODO"*)
  (*let open_process_args_out _ = failwith "TODO"*)
  (*let open_process_args_full _ = failwith "TODO"*)
  (*let process_in_pid _ = failwith "TODO"*)
  (*let process_out_pid _ = failwith "TODO"*)
  (*let process_pid _ = failwith "TODO"*)
  (*let process_full_pid _ = failwith "TODO"*)
  (*let close_process_in _ = failwith "TODO"*)
  (*let close_process_out _ = failwith "TODO"*)
  (*let close_process _ = failwith "TODO"*)
  (*let close_process_full _ = failwith "TODO"*)

  (* *)

  (*let select _ = failwith "TODO"*)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/lockf.html *)
  let lockf fd lock_command length =
    Unix.lockf (Picos_fd.unsafe_get fd) lock_command length

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html *)
  let socket ?cloexec socket_domain socket_type protocol =
    rcn (Unix.socket ?cloexec socket_domain socket_type protocol)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/socketpair.html *)
  let socketpair ?cloexec socket_domain socket_type mystery =
    let fst, snd = Unix.socketpair ?cloexec socket_domain socket_type mystery in
    (rcn fst, rcn snd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html *)
  let accept ?cloexec fd =
    let fd, sockaddr = again_cloexec_0 ?cloexec fd Unix.accept `R in
    (rcn fd, sockaddr)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/bind.html *)
  let bind fd sockaddr = progress_1 fd sockaddr Unix.bind `W "bind"

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html *)
  let connect fd sockaddr = progress_1 fd sockaddr Unix.connect `W "connect"

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/listen.html *)
  let listen fd max_pending = Unix.listen (Picos_fd.unsafe_get fd) max_pending

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/shutdown.html *)
  let shutdown fd shutdown_command =
    Unix.shutdown (Picos_fd.unsafe_get fd) shutdown_command

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockname.html *)
  let getsockname fd = Unix.getsockname (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/getpeername.html *)
  let getpeername fd = Unix.getpeername (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/recv.html *)
  let recv fd bytes offset length flags =
    again_4 fd bytes offset length flags Unix.recv
      (if List.exists is_oob flags then `E else `R)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/recvfrom.html *)
  let recvfrom fd bytes offset length flags =
    again_4 fd bytes offset length flags Unix.recvfrom
      (if List.exists is_oob flags then `E else `R)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/send.html *)
  let send fd bytes offset length flags =
    again_4 fd bytes offset length flags Unix.send `W

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/send.html *)
  let send_substring fd string offset length flags =
    again_4 fd string offset length flags Unix.send_substring `W

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/sendto.html *)
  let sendto fd bytes offset length flags sockaddr =
    again_5 fd bytes offset length flags sockaddr Unix.sendto `W

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/sendto.html *)
  let sendto_substring fd string offset length flags sockaddr =
    again_5 fd string offset length flags sockaddr Unix.sendto_substring `W

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html *)
  let getsockopt fd option = Unix.getsockopt (Picos_fd.unsafe_get fd) option

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/setsockopt.html *)
  let setsockopt fd option bool =
    Unix.setsockopt (Picos_fd.unsafe_get fd) option bool

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html *)
  let getsockopt_int fd option =
    Unix.getsockopt_int (Picos_fd.unsafe_get fd) option

  let setsockopt_int fd option int =
    Unix.setsockopt_int (Picos_fd.unsafe_get fd) option int

  let getsockopt_optint fd option =
    Unix.getsockopt_optint (Picos_fd.unsafe_get fd) option

  let setsockopt_optint fd option optint =
    Unix.setsockopt_optint (Picos_fd.unsafe_get fd) option optint

  let getsockopt_float fd option =
    Unix.getsockopt_float (Picos_fd.unsafe_get fd) option

  let setsockopt_float fd option float =
    Unix.setsockopt_float (Picos_fd.unsafe_get fd) option float

  let getsockopt_error fd = Unix.getsockopt_error (Picos_fd.unsafe_get fd)

  (* *)

  (*let open_connection _ = failwith "TODO"*)
  (*let shutdown_connection _ = failwith "TODO"*)
  (*let establish_server _ = failwith "TODO"*)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcgetattr.html *)
  let tcgetattr fd = Unix.tcgetattr (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcsetattr.html *)
  let tcsetattr fd setattr_when terminal_io =
    Unix.tcsetattr (Picos_fd.unsafe_get fd) setattr_when terminal_io

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcsendbreak.html *)
  let tcsendbreak fd duration =
    Unix.tcsendbreak (Picos_fd.unsafe_get fd) duration

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcdrain.html *)
  let tcdrain fd = Unix.tcdrain (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcflush.html *)
  let tcflush fd flush_queue = Unix.tcflush (Picos_fd.unsafe_get fd) flush_queue

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/tcflow.html *)
  let tcflow fd flow_action = Unix.tcflow (Picos_fd.unsafe_get fd) flow_action
end
