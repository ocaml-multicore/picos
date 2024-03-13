(** A transparently asynchronous [Unix] module for {!Picos}. *)

include module type of Unix

val await : file_descr -> [ `R | `W | `E ] -> file_descr
(** [await file_descr op] awaits for [file_descr] to be ready for [op] and
    returns [file_descr]. *)
