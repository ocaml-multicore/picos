(** Externally reference counted {{!Unix.file_descr} file descriptors}. *)

val report_leaks : bool ref
(** If [!report_leaks = true] then undisposed file descriptors will be reported
    {{!Stdlib.at_exit} at exit}. *)

include Picos_aux_rc.S with type Resource.t = Unix.file_descr
