module type Intf = sig
  type t = { exn : exn; bt : Printexc.raw_backtrace }
  (** An exception and a backtrace. *)

  val get : exn -> t
  (** [get exn] is equivalent to
      [{ exn; bt = Printexc.get_raw_backtrace () }]. *)

  val get_callstack : int -> exn -> t
  (** [get_callstack n exn] is equivalent to
      [{ exn; bt = Printexc.get_callstack n }].

      Note that [Printexc.get_callstack 0] effectively returns a constant value
      and this function is optimized to take that into account. *)

  val raise : t -> 'a
  (** [raise exn_bt] is equivalent to
      [Printexc.raise_with_backtrace exn_bt.exn exn_bt.bt]. *)
end

type t = { exn : exn; bt : Printexc.raw_backtrace }
