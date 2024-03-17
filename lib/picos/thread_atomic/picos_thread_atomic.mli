(** Operations on [ref]s that are atomic with respect to systhreads.

    ⚠️ These operations are not parallelism safe. *)

val compare_and_set : 'a ref -> 'a -> 'a -> bool
val exchange : 'a ref -> 'a -> 'a
