open Picos

let nonblock_fds = Picos_htbl.create ~hashed_type:(module Picos_fd.Resource) ()

module Unix = struct
  include Unix

  type file_descr = Picos_fd.t

  let is_oob flag = MSG_OOB == flag
  let is_nonblock flag = O_NONBLOCK == flag

  (* The retry wrappers below are written to avoid closure allocations. *)

  (* [EAGAIN] (and [EWOULDBLOCK]) indicates that the operation would have
     blocked and so we await using the [select] thread for the file descriptor.

     [EINTR] indicates that the operation was interrupted by a signal, which
     usually shouldn't happen with non-blocking operations.  We don't want to
     block, so we do the same thing as with [EAGAIN]. *)

  let[@inline] intr_req fd =
    if Sys.win32 || Picos_htbl.mem nonblock_fds (Picos_fd.unsafe_get fd) then
      Picos_select.Intr.nothing
    else Picos_select.Intr.req ~seconds:0.000_01 (* 10μs - TODO *)

  let rec again_0 fd fn op =
    let intr = intr_req fd in
    match fn (Picos_fd.unsafe_get fd) with
    | result ->
        Picos_select.Intr.clr intr;
        result
    | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
        Picos_select.Intr.clr intr;
        again_0 (Picos_select.await_on fd op) fn op
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  let rec again_cloexec_0 ?cloexec fd fn op =
    let intr = intr_req fd in
    match fn ?cloexec (Picos_fd.unsafe_get fd) with
    | result ->
        Picos_select.Intr.clr intr;
        result
    | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
        Picos_select.Intr.clr intr;
        again_cloexec_0 ?cloexec (Picos_select.await_on fd op) fn op
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  let rec again_3 fd x1 x2 x3 fn op =
    let intr = intr_req fd in
    match fn (Picos_fd.unsafe_get fd) x1 x2 x3 with
    | result ->
        Picos_select.Intr.clr intr;
        result
    | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
        Picos_select.Intr.clr intr;
        again_3 (Picos_select.await_on fd op) x1 x2 x3 fn op
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  let rec again_4 fd x1 x2 x3 x4 fn op =
    let intr = intr_req fd in
    match fn (Picos_fd.unsafe_get fd) x1 x2 x3 x4 with
    | result ->
        Picos_select.Intr.clr intr;
        result
    | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
        Picos_select.Intr.clr intr;
        again_4 (Picos_select.await_on fd op) x1 x2 x3 x4 fn op
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  let rec again_5 fd x1 x2 x3 x4 x5 fn op =
    let intr = intr_req fd in
    match fn (Picos_fd.unsafe_get fd) x1 x2 x3 x4 x5 with
    | result ->
        Picos_select.Intr.clr intr;
        result
    | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
        Picos_select.Intr.clr intr;
        again_5 (Picos_select.await_on fd op) x1 x2 x3 x4 x5 fn op
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  (* [EINPROGRESS] indicates that a socket operation is being performed
     asynchronously.  We await using the [select] thread for the operation to
     complete and then get the error from the socket. *)

  let progress_1 fd x1 fn op name =
    let intr = intr_req fd in
    match fn (Picos_fd.unsafe_get fd) x1 with
    | () -> Picos_select.Intr.clr intr
    | exception
        Unix.Unix_error ((EAGAIN | EINPROGRESS | EINTR | EWOULDBLOCK), _, _) ->
      begin
        (* The documentation of [bind] and [connect] does not mention [EAGAIN]
           (or [EWOULDBLOCK]), but on Windows we do seem to get those errors
           from [connect].

           The documentation of [bind] does not mention [EINTR].  Matching on
           that shouldn't cause issues with [bind].

           For [connect] both [EINPROGRESS] and [EINTR] mean that connection
           will be established asynchronously and we use [select] to wait. *)
        Picos_select.Intr.clr intr;
        let fd = Picos_select.await_on fd op in
        match Unix.getsockopt_error (Picos_fd.unsafe_get fd) with
        | None -> ()
        | Some error -> raise (Unix.Unix_error (error, name, ""))
      end
    | exception exn ->
        Picos_select.Intr.clr intr;
        raise exn

  let stdin = Picos_fd.create ~dispose:false Unix.stdin
  and stdout = Picos_fd.create ~dispose:false Unix.stdout
  and stderr = Picos_fd.create ~dispose:false Unix.stderr

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/open.html *)
  let openfile path flags file_perm =
    let fd = Picos_fd.create (Unix.openfile path flags file_perm) in
    if List.exists is_nonblock flags then begin
      let if_not_added_fd_has_been_closed_outside =
        Picos_htbl.try_add nonblock_fds (Picos_fd.unsafe_get fd) ()
      in
      assert if_not_added_fd_has_been_closed_outside
    end;
    fd

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/close.html *)
  let close fd =
    let _ : bool =
      Picos_htbl.try_remove nonblock_fds (Picos_fd.unsafe_get fd)
    in
    Picos_fd.decr ~close:true fd

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
  let dup ?cloexec fd =
    Picos_fd.create (Unix.dup ?cloexec (Picos_fd.unsafe_get fd))

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/dup.html *)
  let dup2 ?cloexec src dst =
    Unix.dup2 ?cloexec (Picos_fd.unsafe_get src) (Picos_fd.unsafe_get dst)

  let set_nonblock fd =
    Unix.set_nonblock (Picos_fd.unsafe_get fd);
    Picos_htbl.try_add nonblock_fds (Picos_fd.unsafe_get fd) () |> ignore

  let clear_nonblock fd =
    Unix.clear_nonblock (Picos_fd.unsafe_get fd);
    Picos_htbl.try_remove nonblock_fds (Picos_fd.unsafe_get fd) |> ignore

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fcntl.html *)
  let set_close_on_exec fd = Unix.set_close_on_exec (Picos_fd.unsafe_get fd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/fcntl.html *)
  let clear_close_on_exec fd = Unix.clear_close_on_exec (Picos_fd.unsafe_get fd)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/pipe.html *)
  let pipe ?cloexec () =
    let inn, out = Unix.pipe ?cloexec () in
    (Picos_fd.create inn, Picos_fd.create out)

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

  module Wait_flag = struct
    let nohang_bit = 0b10
    let untraced_bit = 0b01

    (* Note that this is optimized to the identity function. *)
    let to_int = function WNOHANG -> 0 | WUNTRACED -> 1
    let to_bit flag = nohang_bit - to_int flag
    let () = assert (to_bit WNOHANG = nohang_bit)
    let () = assert (to_bit WUNTRACED = untraced_bit)

    let rec to_bits flags bits =
      match flags with
      | [] -> bits
      | flag :: flags -> to_bits flags (bits lor to_bit flag)

    let to_bits flags = to_bits flags 0
    let to_flags = [| []; [ WUNTRACED ]; [ WNOHANG ]; [ WNOHANG; WUNTRACED ] |]
    let to_flags bits = Array.unsafe_get to_flags bits
  end

  let rec waitpid_unix ~bits ~pid =
    if bits land Wait_flag.nohang_bit <> 0 then
      Unix.waitpid (Wait_flag.to_flags bits) pid
    else
      let computation = Computation.create () in
      Picos_select.return_on_sigchld computation ();
      match
        Unix.waitpid (Wait_flag.to_flags (bits lor Wait_flag.nohang_bit)) pid
      with
      | exception Unix_error (EINTR, _, _) -> waitpid_unix ~bits ~pid
      | (pid_or_0, _) as result ->
          if pid_or_0 = 0 then begin
            Computation.await computation;
            waitpid_unix ~bits ~pid
          end
          else begin
            Computation.finish computation;
            result
          end
      | exception exn ->
          Computation.finish computation;
          raise exn

  let waitpid_win32 ~bits ~pid =
    if bits land Wait_flag.nohang_bit <> 0 then
      Unix.waitpid (Wait_flag.to_flags bits) pid
    else
      (* One way to provide a scheduler friendly [waitpid] on Windows would be
         to use a thread pool to run blocking operations on.  PR for a thread
         pool implemented in Picos would be welcome! *)
      invalid_arg
        "Picos_stdio.Unix.waitpid is currently not supported on Windows \
         without WNOHANG"

  let waitpid flags pid =
    let bits = Wait_flag.to_bits flags in
    if Sys.win32 then
      if pid <> -1 then waitpid_win32 ~bits ~pid
      else begin
        (* This should raise? *)
        Unix.waitpid flags pid
      end
    else waitpid_unix ~bits ~pid

  let wait () =
    if not Sys.win32 then waitpid_unix ~bits:0 ~pid:(-1)
    else begin
      (* This should raise [Invalid_argument] *)
      Unix.wait ()
    end

  (* *)

  let sh = "/bin/sh"

  let system cmd =
    if Sys.win32 then
      (* One way to provide a scheduler friendly [system] on Windows would be to
         use a thread pool to run blocking operations on.  PR for a thread pool
         implemented in Picos would be welcome! *)
      invalid_arg
        "Picos_stdio.Unix.system is currently not supported on Windows"
    else
      create_process sh [| sh; "-c"; cmd |] stdin stdout stderr
      |> waitpid [] |> snd

  (* *)

  (*let select _ = failwith "TODO"*)

  (* *)

  let exit_bt = Exn_bt.get_callstack 0 Exit

  let sleepf seconds =
    let sleep = Computation.create ~mode:`LIFO () in
    (* We could also use [Computation.cancel_after], but we already depend on
       [Picos_select]. *)
    Picos_select.cancel_after ~seconds sleep exit_bt;
    let trigger = Trigger.create () in
    if Computation.try_attach sleep trigger then
      match Trigger.await trigger with
      | None ->
          (* This means that the timeout was triggered and [sleep] has been
             canceled. *)
          ()
      | Some exn_bt ->
          (* This means that the underlying fiber was canceled.

             Note that the [exn_bt] does not come from [sleep]!

             We must finish the [sleep] computation, which signals the scheduler
             that the timeout is no longer needed. *)
          Computation.finish sleep;
          Exn_bt.raise exn_bt

  let sleep seconds = sleepf (Float.of_int seconds)

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/lockf.html *)
  let lockf fd lock_command length =
    Unix.lockf (Picos_fd.unsafe_get fd) lock_command length

  (* *)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html *)
  let socket ?cloexec socket_domain socket_type protocol =
    Picos_fd.create (Unix.socket ?cloexec socket_domain socket_type protocol)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/socketpair.html *)
  let socketpair ?cloexec socket_domain socket_type mystery =
    let fst, snd = Unix.socketpair ?cloexec socket_domain socket_type mystery in
    (Picos_fd.create fst, Picos_fd.create snd)

  (* https://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html *)
  let accept ?cloexec fd =
    let fd, sockaddr = again_cloexec_0 ?cloexec fd Unix.accept `R in
    (Picos_fd.create fd, sockaddr)

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
