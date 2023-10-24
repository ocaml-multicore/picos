module type Exn_bt = sig
  type t

  (* *)

  val discontinue : ('a, 'b) Effect.Deep.continuation -> t -> 'b
  (** [discontinue k exn_bt] is equivalent to
      [Effect.Deep.discontinue_with_backtrace k exn_bt.exn exn_bt.bt]. *)

  val discontinue_with :
    ('a, 'b) Effect.Shallow.continuation ->
    t ->
    ('b, 'c) Effect.Shallow.handler ->
    'c
  (** [discontinue_with k exn_bt h] is equivalent to
      [Effect.Shallow.discontinue_with_backtrace k exn_bt.exn exn_bt.bt h]. *)
end

module type Trigger = sig
  type _ t
  type exn_bt

  (* *)

  (** Schedulers may handle the {!Await} effect to customize the behavior of
      [await]. *)
  type _ Effect.t +=
    private
    | Await : [ `On | `Signal ] t -> exn_bt option Effect.t
end

module type Computation = sig
  type _ as_cancelable
  type exn_bt

  (* *)

  (** Schedulers may handle the {!Cancel_after} effect to customize the behavior
      of [cancel_after]. *)
  type _ Effect.t +=
    private
    | Cancel_after : {
        seconds : float;
        exn_bt : exn_bt;
        computation : 'a as_cancelable;
      }
        -> unit Effect.t
end

module type Fiber = sig
  type _ t
  type _ as_cancelable

  (* *)

  (** Schedulers may handle the {!Current} effect to customize the behavior of
      [current]. *)
  type _ Effect.t += private Current : [ `Sync | `Async ] t Effect.t

  (** Schedulers may handle the {!Yield} effect to customize the behavior of
      [yield]. *)
  type _ Effect.t += private Yield : unit Effect.t

  (** Schedulers may handle the {!Spawn} effect to customize the behavior of
      [spawn]. *)
  type _ Effect.t +=
    private
    | Spawn : {
        forbid : bool;
        computation : 'a as_cancelable;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t
end
