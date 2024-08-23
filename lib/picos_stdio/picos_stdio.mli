(** Basic IO facilities based on OCaml standard libraries for {!Picos}. *)

(** {1 Modules} *)

module Unix : sig
  (** A transparently asynchronous replacement for a subset of the {{!Deps.Unix}
      [Unix]} module that comes with OCaml.

      In this module operations on file descriptors, such as {!read} and
      {!write} and others, including {!select}, implicitly block, in a scheduler
      friendly manner, to await for the file descriptor to become available for
      the operation.  This works best with file descriptors {{!set_nonblock} set
      to non-blocking mode}.

      In addition to operations on file descriptors, in this module

      - {!sleep}, and
      - {!sleepf}

      also block in a scheduler friendly manner.  Additionally

      - {!wait},
      - {!waitpid}, and
      - {!system}

      also block in a scheduler friendly manner except on Windows.

      ⚠️ Shared (i.e. {{!create_process} inherited or inheritable} or {{!dup}
      duplicated}) file descriptors, such as {!stdin}, {!stdout}, and {!stderr},
      typically should not be put into non-blocking mode, because that affects
      all of the parties using the shared file descriptors.  However, for
      non-shared file descriptors {{!set_nonblock} non-blocking mode} improves
      performance significantly with this module.

      ⚠️ Beware that this does not currently try to work around any limitations
      of the {{!Deps.Unix} [Unix]} module that comes with OCaml.  In particular,
      on Windows, only sockets can be put into non-blocking mode.  Also, on
      Windows, scheduler friendly blocking only works properly with non-blocking
      file descriptors, i.e. sockets.

      ⚠️ This module uses {!Picos_select} and you may need to
      {{!Picos_select.configure} configure} it at start of your application.

      Please consult the documentation of the {{!Deps.Unix} [Unix]} module that
      comes with OCaml. *)

  type file_descr = Picos_fd.t
  (** Opaque type alias for {{!Deps.Unix.file_descr} [Unix.file_descr]}.

      ⚠️ Please consider the reference counting of file descriptors as an
      internal implementation detail and avoid depending on it. *)

  val close : file_descr -> unit
  (** [close file_descr] marks the file descriptor as to be closed.

      ℹ️ The file descriptor will either be closed immediately or after all
      concurrently running transparently asynchronous operations with the file
      descriptor have finished.

      ⚠️ After calling [close] no new operations should be started with the file
      descriptor. *)

  val close_pair : file_descr * file_descr -> unit
  (** [close_pair (fd1, fd2)] is equivalent to
      {{!close} [close fd1; close fd2]}. *)

  type error = Unix.error =
    | E2BIG
    | EACCES
    | EAGAIN
    | EBADF
    | EBUSY
    | ECHILD
    | EDEADLK
    | EDOM
    | EEXIST
    | EFAULT
    | EFBIG
    | EINTR
    | EINVAL
    | EIO
    | EISDIR
    | EMFILE
    | EMLINK
    | ENAMETOOLONG
    | ENFILE
    | ENODEV
    | ENOENT
    | ENOEXEC
    | ENOLCK
    | ENOMEM
    | ENOSPC
    | ENOSYS
    | ENOTDIR
    | ENOTEMPTY
    | ENOTTY
    | ENXIO
    | EPERM
    | EPIPE
    | ERANGE
    | EROFS
    | ESPIPE
    | ESRCH
    | EXDEV
    | EWOULDBLOCK
    | EINPROGRESS
    | EALREADY
    | ENOTSOCK
    | EDESTADDRREQ
    | EMSGSIZE
    | EPROTOTYPE
    | ENOPROTOOPT
    | EPROTONOSUPPORT
    | ESOCKTNOSUPPORT
    | EOPNOTSUPP
    | EPFNOSUPPORT
    | EAFNOSUPPORT
    | EADDRINUSE
    | EADDRNOTAVAIL
    | ENETDOWN
    | ENETUNREACH
    | ENETRESET
    | ECONNABORTED
    | ECONNRESET
    | ENOBUFS
    | EISCONN
    | ENOTCONN
    | ESHUTDOWN
    | ETOOMANYREFS
    | ETIMEDOUT
    | ECONNREFUSED
    | EHOSTDOWN
    | EHOSTUNREACH
    | ELOOP
    | EOVERFLOW
    | EUNKNOWNERR of int

  exception Unix_error of error * string * string

  val error_message : error -> string
  val handle_unix_error : ('a -> 'b) -> 'a -> 'b
  val environment : unit -> string array
  val unsafe_environment : unit -> string array
  val getenv : string -> string
  val unsafe_getenv : string -> string
  val putenv : string -> string -> unit

  type process_status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  type wait_flag = Unix.wait_flag = WNOHANG | WUNTRACED

  val execv : string -> string array -> 'a
  val execve : string -> string array -> string array -> 'a
  val execvp : string -> string array -> 'a
  val execvpe : string -> string array -> string array -> 'a
  val fork : unit -> int
  val wait : unit -> int * process_status
  val waitpid : wait_flag list -> int -> int * process_status
  val system : string -> process_status
  val _exit : int -> 'a
  val getpid : unit -> int
  val getppid : unit -> int
  val nice : int -> int
  val stdin : file_descr
  val stdout : file_descr
  val stderr : file_descr

  type open_flag = Unix.open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_APPEND
    | O_CREAT
    | O_TRUNC
    | O_EXCL
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC
    | O_KEEPEXEC

  type file_perm = int

  val openfile : string -> open_flag list -> file_perm -> file_descr
  val fsync : file_descr -> unit
  val read : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> int
  val single_write : file_descr -> bytes -> int -> int -> int
  val write_substring : file_descr -> string -> int -> int -> int
  val single_write_substring : file_descr -> string -> int -> int -> int

  (*val in_channel_of_descr : file_descr -> In_channel.t*)
  (*val out_channel_of_descr : file_descr -> Out_channel.t*)
  (*val descr_of_in_channel : In_channel.t -> file_descr*)
  (*val descr_of_out_channel : Out_channel.t -> file_descr*)

  type seek_command = Unix.seek_command = SEEK_SET | SEEK_CUR | SEEK_END

  val lseek : file_descr -> int -> seek_command -> int
  val truncate : string -> int -> unit
  val ftruncate : file_descr -> int -> unit

  type file_kind = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  type stats = Unix.stats = {
    st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int;
    st_atime : float;
    st_mtime : float;
    st_ctime : float;
  }

  val stat : string -> stats
  val lstat : string -> stats
  val fstat : file_descr -> stats
  val isatty : file_descr -> bool

  module LargeFile : sig
    val lseek : file_descr -> int64 -> seek_command -> int64
    val truncate : string -> int64 -> unit
    val ftruncate : file_descr -> int64 -> unit

    type stats = Unix.LargeFile.stats = {
      st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int64;
      st_atime : float;
      st_mtime : float;
      st_ctime : float;
    }

    val stat : string -> stats
    val lstat : string -> stats
    val fstat : file_descr -> stats
  end

  val map_file :
    file_descr ->
    ?pos:int64 ->
    ('a, 'b) Bigarray.kind ->
    'c Bigarray.layout ->
    bool ->
    int array ->
    ('a, 'b, 'c) Bigarray.Genarray.t

  val unlink : string -> unit
  val rename : string -> string -> unit
  val link : ?follow:bool -> string -> string -> unit
  val realpath : string -> string

  type access_permission = Unix.access_permission = R_OK | W_OK | X_OK | F_OK

  val chmod : string -> file_perm -> unit
  val fchmod : file_descr -> file_perm -> unit
  val chown : string -> int -> int -> unit
  val fchown : file_descr -> int -> int -> unit
  val umask : file_perm -> file_perm
  val access : string -> access_permission list -> unit
  val dup : ?cloexec:bool -> file_descr -> file_descr
  val dup2 : ?cloexec:bool -> file_descr -> file_descr -> unit
  val set_nonblock : file_descr -> unit
  val clear_nonblock : file_descr -> unit
  val set_close_on_exec : file_descr -> unit
  val clear_close_on_exec : file_descr -> unit
  val mkdir : string -> file_perm -> unit
  val rmdir : string -> unit
  val chdir : string -> unit
  val getcwd : unit -> string
  val chroot : string -> unit

  type dir_handle = Unix.dir_handle

  val opendir : string -> dir_handle
  val readdir : dir_handle -> string
  val rewinddir : dir_handle -> unit
  val closedir : dir_handle -> unit
  val pipe : ?cloexec:bool -> unit -> file_descr * file_descr
  val mkfifo : string -> file_perm -> unit

  val create_process :
    string -> string array -> file_descr -> file_descr -> file_descr -> int

  val create_process_env :
    string ->
    string array ->
    string array ->
    file_descr ->
    file_descr ->
    file_descr ->
    int

  (*val open_process_in : string -> In_channel.t*)
  (*val open_process_out : string -> Out_channel.t*)
  (*val open_process : string -> In_channel.t * Out_channel.t*)
  (*val open_process_full :
    string -> string array -> In_channel.t * Out_channel.t * In_channel.t*)
  (*val open_process_args : string -> string array -> In_channel.t * Out_channel.t*)
  (*val open_process_args_in : string -> string array -> In_channel.t*)
  (*val open_process_args_out : string -> string array -> Out_channel.t*)
  (*val open_process_args_full :
    string ->
    string array ->
    string array ->
    In_channel.t * Out_channel.t * In_channel.t*)
  (*val process_in_pid : In_channel.t -> int*)
  (*val process_out_pid : Out_channel.t -> int*)
  (*val process_pid : In_channel.t * Out_channel.t -> int*)
  (*val process_full_pid : In_channel.t * Out_channel.t * In_channel.t -> int*)
  (*val close_process_in : In_channel.t -> process_status*)
  (*val close_process_out : Out_channel.t -> process_status*)
  (*val close_process : In_channel.t * Out_channel.t -> process_status*)

  (*val close_process_full :
    In_channel.t * Out_channel.t * In_channel.t -> process_status*)

  val symlink : ?to_dir:bool -> string -> string -> unit
  val has_symlink : unit -> bool
  val readlink : string -> string

  val select :
    file_descr list ->
    file_descr list ->
    file_descr list ->
    float ->
    file_descr list * file_descr list * file_descr list
  (** [select rds wrs exs timeout] is like {!Deps.Unix.select}, but uses
      {!Picos_select} to avoid blocking the thread.

      🐌 You may find composing multi file descriptor awaits via other means
      with {!Picos_select} more flexible and efficient. *)

  type lock_command = Unix.lock_command =
    | F_ULOCK
    | F_LOCK
    | F_TLOCK
    | F_TEST
    | F_RLOCK
    | F_TRLOCK

  val lockf : file_descr -> lock_command -> int -> unit
  val kill : int -> int -> unit

  type sigprocmask_command = Unix.sigprocmask_command =
    | SIG_SETMASK
    | SIG_BLOCK
    | SIG_UNBLOCK

  val sigprocmask : sigprocmask_command -> int list -> int list
  val sigpending : unit -> int list
  (*val sigsuspend : int list -> unit*)
  (*val pause : unit -> unit*)

  type process_times = Unix.process_times = {
    tms_utime : float;
    tms_stime : float;
    tms_cutime : float;
    tms_cstime : float;
  }

  type tm = Unix.tm = {
    tm_sec : int;
    tm_min : int;
    tm_hour : int;
    tm_mday : int;
    tm_mon : int;
    tm_year : int;
    tm_wday : int;
    tm_yday : int;
    tm_isdst : bool;
  }

  val time : unit -> float
  val gettimeofday : unit -> float
  val gmtime : float -> tm
  val localtime : float -> tm
  val mktime : tm -> float * tm
  val alarm : int -> int
  val sleep : int -> unit
  val sleepf : float -> unit
  val times : unit -> process_times
  val utimes : string -> float -> float -> unit

  type interval_timer = Unix.interval_timer =
    | ITIMER_REAL
    | ITIMER_VIRTUAL
    | ITIMER_PROF

  type interval_timer_status = Unix.interval_timer_status = {
    it_interval : float;
    it_value : float;
  }

  val getitimer : interval_timer -> interval_timer_status

  val setitimer :
    interval_timer -> interval_timer_status -> interval_timer_status

  val getuid : unit -> int
  val geteuid : unit -> int
  val setuid : int -> unit
  val getgid : unit -> int
  val getegid : unit -> int
  val setgid : int -> unit
  val getgroups : unit -> int array
  val setgroups : int array -> unit
  val initgroups : string -> int -> unit

  type passwd_entry = Unix.passwd_entry = {
    pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string;
  }

  type group_entry = Unix.group_entry = {
    gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array;
  }

  val getlogin : unit -> string
  val getpwnam : string -> passwd_entry
  val getgrnam : string -> group_entry
  val getpwuid : int -> passwd_entry
  val getgrgid : int -> group_entry

  type inet_addr = Unix.inet_addr

  val inet_addr_of_string : string -> inet_addr
  val string_of_inet_addr : inet_addr -> string
  val inet_addr_any : inet_addr
  val inet_addr_loopback : inet_addr
  val inet6_addr_any : inet_addr
  val inet6_addr_loopback : inet_addr
  val is_inet6_addr : inet_addr -> bool

  type socket_domain = Unix.socket_domain = PF_UNIX | PF_INET | PF_INET6

  type socket_type = Unix.socket_type =
    | SOCK_STREAM
    | SOCK_DGRAM
    | SOCK_RAW
    | SOCK_SEQPACKET

  type sockaddr = Unix.sockaddr =
    | ADDR_UNIX of string
    | ADDR_INET of inet_addr * int

  val socket :
    ?cloexec:bool -> socket_domain -> socket_type -> int -> file_descr

  val domain_of_sockaddr : sockaddr -> socket_domain

  val socketpair :
    ?cloexec:bool ->
    socket_domain ->
    socket_type ->
    int ->
    file_descr * file_descr

  val accept : ?cloexec:bool -> file_descr -> file_descr * sockaddr
  val bind : file_descr -> sockaddr -> unit
  val connect : file_descr -> sockaddr -> unit
  val listen : file_descr -> int -> unit

  type shutdown_command = Unix.shutdown_command =
    | SHUTDOWN_RECEIVE
    | SHUTDOWN_SEND
    | SHUTDOWN_ALL

  val shutdown : file_descr -> shutdown_command -> unit
  val getsockname : file_descr -> sockaddr
  val getpeername : file_descr -> sockaddr

  type msg_flag = Unix.msg_flag = MSG_OOB | MSG_DONTROUTE | MSG_PEEK

  val recv : file_descr -> bytes -> int -> int -> msg_flag list -> int

  val recvfrom :
    file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr

  val send : file_descr -> bytes -> int -> int -> msg_flag list -> int

  val send_substring :
    file_descr -> string -> int -> int -> msg_flag list -> int

  val sendto :
    file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int

  val sendto_substring :
    file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int

  type socket_bool_option = Unix.socket_bool_option =
    | SO_DEBUG
    | SO_BROADCAST
    | SO_REUSEADDR
    | SO_KEEPALIVE
    | SO_DONTROUTE
    | SO_OOBINLINE
    | SO_ACCEPTCONN
    | TCP_NODELAY
    | IPV6_ONLY
    | SO_REUSEPORT

  type socket_int_option = Unix.socket_int_option =
    | SO_SNDBUF
    | SO_RCVBUF
    | SO_ERROR [@ocaml.deprecated "Use Unix.getsockopt_error instead."]
    | SO_TYPE
    | SO_RCVLOWAT
    | SO_SNDLOWAT

  type socket_optint_option = Unix.socket_optint_option = SO_LINGER

  type socket_float_option = Unix.socket_float_option =
    | SO_RCVTIMEO
    | SO_SNDTIMEO

  val getsockopt : file_descr -> socket_bool_option -> bool
  val setsockopt : file_descr -> socket_bool_option -> bool -> unit
  val getsockopt_int : file_descr -> socket_int_option -> int
  val setsockopt_int : file_descr -> socket_int_option -> int -> unit
  val getsockopt_optint : file_descr -> socket_optint_option -> int option

  val setsockopt_optint :
    file_descr -> socket_optint_option -> int option -> unit

  val getsockopt_float : file_descr -> socket_float_option -> float
  val setsockopt_float : file_descr -> socket_float_option -> float -> unit
  val getsockopt_error : file_descr -> error option

  (*val open_connection : sockaddr -> In_channel.t * Out_channel.t*)
  (*val shutdown_connection : In_channel.t -> unit*)
  (*val establish_server :
    (in_channel -> Out_channel.t -> unit) -> sockaddr -> unit*)

  type host_entry = Unix.host_entry = {
    h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array;
  }

  type protocol_entry = Unix.protocol_entry = {
    p_name : string;
    p_aliases : string array;
    p_proto : int;
  }

  type service_entry = Unix.service_entry = {
    s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string;
  }

  val gethostname : unit -> string
  val gethostbyname : string -> host_entry
  val gethostbyaddr : inet_addr -> host_entry
  val getprotobyname : string -> protocol_entry
  val getprotobynumber : int -> protocol_entry
  val getservbyname : string -> string -> service_entry
  val getservbyport : int -> string -> service_entry

  type addr_info = Unix.addr_info = {
    ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string;
  }

  type getaddrinfo_option = Unix.getaddrinfo_option =
    | AI_FAMILY of socket_domain
    | AI_SOCKTYPE of socket_type
    | AI_PROTOCOL of int
    | AI_NUMERICHOST
    | AI_CANONNAME
    | AI_PASSIVE

  val getaddrinfo :
    string -> string -> getaddrinfo_option list -> addr_info list

  type name_info = Unix.name_info = {
    ni_hostname : string;
    ni_service : string;
  }

  type getnameinfo_option = Unix.getnameinfo_option =
    | NI_NOFQDN
    | NI_NUMERICHOST
    | NI_NAMEREQD
    | NI_NUMERICSERV
    | NI_DGRAM

  val getnameinfo : sockaddr -> getnameinfo_option list -> name_info

  type terminal_io = Unix.terminal_io = {
    mutable c_ignbrk : bool;
    mutable c_brkint : bool;
    mutable c_ignpar : bool;
    mutable c_parmrk : bool;
    mutable c_inpck : bool;
    mutable c_istrip : bool;
    mutable c_inlcr : bool;
    mutable c_igncr : bool;
    mutable c_icrnl : bool;
    mutable c_ixon : bool;
    mutable c_ixoff : bool;
    mutable c_opost : bool;
    mutable c_obaud : int;
    mutable c_ibaud : int;
    mutable c_csize : int;
    mutable c_cstopb : int;
    mutable c_cread : bool;
    mutable c_parenb : bool;
    mutable c_parodd : bool;
    mutable c_hupcl : bool;
    mutable c_clocal : bool;
    mutable c_isig : bool;
    mutable c_icanon : bool;
    mutable c_noflsh : bool;
    mutable c_echo : bool;
    mutable c_echoe : bool;
    mutable c_echok : bool;
    mutable c_echonl : bool;
    mutable c_vintr : char;
    mutable c_vquit : char;
    mutable c_verase : char;
    mutable c_vkill : char;
    mutable c_veof : char;
    mutable c_veol : char;
    mutable c_vmin : int;
    mutable c_vtime : int;
    mutable c_vstart : char;
    mutable c_vstop : char;
  }

  val tcgetattr : file_descr -> terminal_io

  type setattr_when = Unix.setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

  val tcsetattr : file_descr -> setattr_when -> terminal_io -> unit
  val tcsendbreak : file_descr -> int -> unit
  val tcdrain : file_descr -> unit

  type flush_queue = Unix.flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH

  val tcflush : file_descr -> flush_queue -> unit

  type flow_action = Unix.flow_action = TCOOFF | TCOON | TCIOFF | TCION

  val tcflow : file_descr -> flow_action -> unit
  val setsid : unit -> int
end

(** {1 Examples}

    First we open some modules for convenience:

    {[
      open Picos_finally
      open Picos_structured
      open Picos_stdio
    ]}

    {2 A pair of pipes}

    Here is a simple example of two fibers communicating through a pair of
    pipes:

    {[
      # Picos_randos.run_on ~n_domains:2 @@ fun () ->

        let@ msg_i, msg_o =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair ~cloexec:true
            PF_UNIX SOCK_STREAM 0
        in
        let@ syn_i, syn_o =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair ~cloexec:true
            PF_UNIX SOCK_STREAM 0
        in

        Unix.set_nonblock msg_i;
        Unix.set_nonblock msg_o;
        Unix.set_nonblock syn_i;
        Unix.set_nonblock syn_o;

        Flock.join_after ~on_return:`Terminate begin fun () ->
          Flock.fork begin fun () ->
            let bytes = Bytes.create 100 in
            while true do
              let n =
                Unix.read msg_i bytes 0 100
              in
              if n > 0 then begin
                Printf.printf "%s\n%!"
                  (Bytes.sub_string bytes 0 n);
                let w =
                  Unix.write_substring
                    syn_o "!" 0 1
                in
                assert (w = 1)
              end
            done
          end;

          let send_string s =
            let n = String.length s in
            let w =
              Unix.write_substring msg_o s 0 n
            in
            assert (w = n);
            let r =
              Unix.read syn_i
                (Bytes.create 1) 0 1
            in
            assert (r = 1)
          in

          send_string "Hello, world!";
          send_string "POSIX with OCaml";
        end
      Hello, world!
      POSIX with OCaml
      - : unit = ()
    ]} *)
