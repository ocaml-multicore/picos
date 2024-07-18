open Multicore_bench
open Picos_structured
open Picos_sync
module Queue = Stdlib.Queue

module Bounded_q : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
end = struct
  type 'a t = {
    mutex : Mutex.t;
    queue : 'a Queue.t;
    capacity : int;
    not_empty : Condition.t;
    not_full : Condition.t;
  }

  let create ?(capacity = Int.max_int) () =
    if capacity < 0 then invalid_arg "negative capacity"
    else
      let mutex = Mutex.create ~padded:true ()
      and queue = Queue.create () |> Multicore_magic.copy_as_padded
      and not_empty = Condition.create ~padded:true ()
      and not_full = Condition.create ~padded:true () in
      { mutex; queue; capacity; not_empty; not_full }
      |> Multicore_magic.copy_as_padded

  let is_empty t =
    Mutex.lock ~checked:false t.mutex;
    let result = Queue.is_empty t.queue in
    Mutex.unlock ~checked:false t.mutex;
    result

  let is_full_unsafe t = t.capacity <= Queue.length t.queue

  let push t x =
    Mutex.lock ~checked:false t.mutex;
    match
      while is_full_unsafe t do
        Condition.wait t.not_full t.mutex
      done
    with
    | () ->
        Queue.push x t.queue;
        let n = Queue.length t.queue in
        Mutex.unlock ~checked:false t.mutex;
        if n = 1 then Condition.broadcast t.not_empty
    | exception exn ->
        Mutex.unlock ~checked:false t.mutex;
        raise exn

  let pop t =
    Mutex.lock ~checked:false t.mutex;
    match
      while Queue.length t.queue = 0 do
        Condition.wait t.not_empty t.mutex
      done
    with
    | () ->
        let n = Queue.length t.queue in
        let elem = Queue.pop t.queue in
        Mutex.unlock ~checked:false t.mutex;
        if n = t.capacity then Condition.broadcast t.not_full;
        elem
    | exception exn ->
        Mutex.unlock ~checked:false t.mutex;
        raise exn

  let pop_opt t =
    Mutex.lock ~checked:false t.mutex;
    let n = Queue.length t.queue in
    let elem_opt = Queue.take_opt t.queue in
    Mutex.unlock ~checked:false t.mutex;
    if n = t.capacity then Condition.broadcast t.not_full;
    elem_opt
end

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Bounded_q.create () in

  let op push =
    if push then Bounded_q.push t 101 else Bounded_q.pop_opt t |> ignore
  in

  let init _ =
    assert (Scheduler.run @@ fun () -> Bounded_q.is_empty t);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let wrap _ _ = Scheduler.run in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

(** This will keep a domain running. *)
let yielder () =
  while true do
    Control.yield ()
  done

let run_one ~budgetf ~n_adders ~n_takers ?(n_msgs = 50 * Util.iter_factor) () =
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
    Bundle.join_after @@ fun bundle ->
    Bundle.fork bundle yielder;
    if domain_index < n_adders then
      let rec work () =
        let n = Countdown.alloc n_msgs_to_add ~domain_index ~batch:100 in
        if 0 < n then begin
          for i = 1 to n do
            Bounded_q.push t i
          done;
          work ()
        end
        else Bundle.terminate bundle
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
        else Bundle.terminate bundle
      in
      work ()
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s" (format "adder" n_adders) (format "taker" n_takers)
  in

  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2; 4 ] [ 1; 2; 4 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       if Picos_domain.recommended_domain_count () < n_adders + n_takers then []
       else run_one ~budgetf ~n_adders ~n_takers ())
