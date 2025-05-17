open Multicore_bench
open Picos_std_structured
open Picos_std_sync
module Queue = Stdlib.Queue

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

module Bounded_q : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
end = struct
  type 'a t = {
    lock : Lock.t;
    queue : 'a Queue.t;
    capacity : int;
    not_empty : Lock.Condition.t;
    not_full : Lock.Condition.t;
  }

  let create ?(capacity = Int.max_int) () =
    if capacity < 0 then invalid_arg "negative capacity"
    else
      let lock = Lock.create ~padded:true ()
      and queue = Queue.create () |> Multicore_magic.copy_as_padded
      and not_empty = Lock.Condition.create ~padded:true ()
      and not_full = Lock.Condition.create ~padded:true () in
      { lock; queue; capacity; not_empty; not_full }
      |> Multicore_magic.copy_as_padded

  let is_empty t =
    Lock.acquire t.lock;
    let result = Queue.is_empty t.queue in
    Lock.release t.lock;
    result

  let is_full_unsafe t = t.capacity <= Queue.length t.queue

  let push t x =
    let was_full = ref false in
    Lock.acquire t.lock;
    match
      while is_full_unsafe t do
        was_full := true;
        Lock.Condition.wait t.not_full t.lock
      done
    with
    | () ->
        Queue.push x t.queue;
        let n = Queue.length t.queue in
        Lock.release t.lock;
        if n = 1 then Lock.Condition.signal t.not_empty;
        if !was_full && n < t.capacity then Lock.Condition.signal t.not_full
    | exception exn ->
        Lock.release t.lock;
        raise exn

  let pop t =
    let was_empty = ref false in
    Lock.acquire t.lock;
    match
      while Queue.length t.queue = 0 do
        was_empty := true;
        Lock.Condition.wait t.not_empty t.lock
      done
    with
    | () ->
        let n = Queue.length t.queue in
        let elem = Queue.pop t.queue in
        Lock.release t.lock;
        if n = t.capacity then Lock.Condition.signal t.not_full;
        if !was_empty && 1 < n then Lock.Condition.signal t.not_empty;
        elem
    | exception exn ->
        Lock.release t.lock;
        raise exn

  let pop_opt t =
    Lock.acquire t.lock;
    let n = Queue.length t.queue in
    let elem_opt = Queue.take_opt t.queue in
    Lock.release t.lock;
    if n = t.capacity then Lock.Condition.signal t.not_full;
    elem_opt
end

let run_one_domain ~budgetf ?(n_msgs = 25 * Util.iter_factor) () =
  let t = Bounded_q.create () in

  let op push =
    if push then Bounded_q.push t (ref push) else Bounded_q.pop_opt t |> ignore
  in

  let init _ =
    assert (Scheduler.run @@ fun () -> Bounded_q.is_empty t);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let wrap _ _ = Scheduler.run in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message"
       ~config:"one domain with Lock"

(** This will keep a domain running. *)
let yielder () =
  while true do
    Control.yield ()
  done

let run_one ~budgetf ~n_adders ~n_takers ?(n_msgs = 10 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Bounded_q.create () in

  let n_msgs_to_take = Countdown.create ~n_domains () in
  let n_msgs_to_add = Countdown.create ~n_domains () in

  let init _ =
    assert (Scheduler.run @@ fun () -> Bounded_q.is_empty t);
    Countdown.non_atomic_set n_msgs_to_take n_msgs;
    Countdown.non_atomic_set n_msgs_to_add n_msgs
  in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    Flock.join_after ~on_return:`Terminate @@ fun () ->
    if not is_ocaml4 then Flock.fork yielder;
    if domain_index < n_adders then
      let rec work () =
        let n = Countdown.alloc n_msgs_to_add ~domain_index ~batch:100 in
        if 0 < n then begin
          for i = 1 to n do
            Bounded_q.push t (ref i)
          done;
          work ()
        end
      in
      work ()
    else
      let rec work () =
        let n = Countdown.alloc n_msgs_to_take ~domain_index ~batch:100 in
        if n <> 0 then begin
          for _ = 1 to n do
            ignore (Bounded_q.pop t)
          done;
          work ()
        end
      in
      work ()
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s with Lock" (format "adder" n_adders)
      (format "taker" n_takers)
  in

  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2; 4 ] [ 1; 2; 4 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       if Picos_domain.recommended_domain_count () < n_adders + n_takers then []
       else run_one ~budgetf ~n_adders ~n_takers ())
