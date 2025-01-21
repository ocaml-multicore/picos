module Awaitable = struct
  type 'a t = { mutable _value : 'a; bits : int }

  external as_atomic : 'a t -> 'a Atomic.t = "%identity"

  let make ?padded value =
    Multicore_magic.copy_as ?padded { _value = value; bits = Random.bits () }

  let make_contended value = make ~padded:true value
  let[@inline] get t = Atomic.get (Sys.opaque_identity (as_atomic t))
  let[@inline] compare_and_set x b a = Atomic.compare_and_set (as_atomic x) b a
  let[@inline] exchange t value = Atomic.exchange (as_atomic t) value
  let[@inline] fetch_and_add t n = Atomic.fetch_and_add (as_atomic t) n
  let[@inline] set t value = exchange t value |> ignore
  let[@inline] incr t = fetch_and_add t 1 |> ignore
  let[@inline] decr t = fetch_and_add t (-1) |> ignore
  let[@inline] fenceless_get t = Multicore_magic.fenceless_get (as_atomic t)

  (* *)

  open Picos

  type 'a awaitable = 'a t

  module Packed = struct
    type t = Packed : 'a awaitable -> t [@@unboxed]

    let equal = ( == )
    let hash (Packed awaitable) = awaitable.bits
  end

  module Awaiters = struct
    type _ tdt =
      | Zero : [> `Zero ] tdt
      | One : {
          awaitable : 'a awaitable; (* Might also want to clear this *)
          mutable value : 'a; (* This is mutable to avoid space leaks *)
          trigger : Trigger.t;
          mutable counter : int;
          mutable next : min0;
        }
          -> [> `One ] tdt
      | Many : { head : is1; prev : is1; tail : is1 } -> [> `Many ] tdt

    and min0 = Min0 : [< `Zero | `One ] tdt -> min0 [@@unboxed]
    and min1 = Min1 : [< `One | `Many ] tdt -> min1 [@@unboxed]
    and is1 = [ `One ] tdt

    let[@inline] exec (Many r : [ `Many ] tdt) =
      if r.prev != r.tail then
        let (One prev_r) = r.prev in
        if prev_r.next == Min0 Zero then prev_r.next <- Min0 r.tail

    let[@inline] snoc t (One tail_r as tail) =
      match t with
      | Min1 (One head_r) ->
          tail_r.counter <- head_r.counter + 1;
          Many { head = One head_r; prev = One head_r; tail }
      | Min1 (Many many_r as many) ->
          exec many;
          let (One prev_r as prev) = many_r.tail in
          tail_r.counter <- prev_r.counter + 1;
          Many { head = many_r.head; prev; tail }

    external as1 : min0 -> is1 = "%identity"

    let[@inline] awaitable_of (One r : is1) = Packed.Packed r.awaitable
    let[@inline] counter_of (One r : is1) = r.counter

    let[@inline] next_of (One r : is1) ~tail =
      let next = as1 r.next in
      let counter = r.counter in
      if counter_of tail - counter < counter_of next - counter then tail
      else next

    let[@inline] set_next_of (One one_r : is1) (next : is1) =
      one_r.next <- Min0 next

    let[@inline] generalize (One r : is1) = One r
    let[@inline] is_signaled (One r : is1) = Trigger.is_signaled r.trigger
    let[@inline] is_signalable (One r : is1) = get r.awaitable != r.value
    let[@inline] await (One r : is1) = Trigger.await r.trigger
    let[@inline] clear (One r : is1) = r.value <- Obj.magic ()

    let[@inline] signal_and_clear (One r : is1) =
      Trigger.signal r.trigger;
      r.value <- Obj.magic ()

    let[@inline] last (one : is1) =
      if is_signaled one then Zero else generalize one

    let[@inline] signal_last_one (one : is1) =
      if is_signalable one then begin
        signal_and_clear one;
        Zero
      end
      else last one

    let cleanup awaiters ~count =
      let count = ref count in
      match awaiters with
      | Min1 (One r) -> last (One r)
      | Min1 (Many many_r as many) ->
          exec many;
          let tail = many_r.tail in
          let head = ref tail in
          let work = ref many_r.head in
          while !work != tail do
            if is_signaled !work then begin
              let next = next_of !work ~tail in
              let n = !count - 1 in
              count := n;
              if n <> 0 then work := next
              else begin
                head := next;
                work := tail
              end
            end
            else begin
              head := !work;
              work := tail
            end
          done;
          let head = !head in
          if head == tail then begin
            last head
          end
          else begin
            if !count <> 0 then begin
              let prev = ref head in
              let work = ref (next_of !prev ~tail) in
              while !work != tail do
                let next = next_of !work ~tail in
                if is_signaled !work then begin
                  set_next_of !prev next;
                  let n = !count - 1 in
                  count := n;
                  if n <> 0 then work := next else work := tail
                end
                else begin
                  prev := !work;
                  work := next
                end
              done;
              if !count <> 0 && is_signaled tail then clear tail
            end;
            Many { head; prev = tail; tail }
          end

    let ( (* test cleanup *) ) =
      if false then begin
        [ 1; Int.max_int ]
        |> List.iter @@ fun count ->
           for n = 1 to 4 do
             for bits = 0 to (1 lsl n) - 1 do
               let make i =
                 let trigger = Trigger.create () in
                 if bits land (1 lsl i) = 0 then Trigger.signal trigger;
                 let awaitable = make 0 and next = Min0 Zero in
                 One { awaitable; value = 1; trigger; counter = 0; next }
               in
               let queue = ref (Min1 (make 0)) in
               for i = 1 to n - 1 do
                 queue := Min1 (snoc !queue (make i))
               done;
               let rec fold zero fn = function
                 | Zero -> zero
                 | One r -> fn zero (One r)
                 | Many { head; tail; _ } as many ->
                     exec many;
                     fn
                       (let head = next_of head ~tail in
                        if head != tail then
                          fold zero fn (Many { head; prev = tail; tail })
                        else fold zero fn (generalize head))
                       head
               in
               let open struct
                 type t = { total : int; signaled : int; initial : int }
               end in
               let stats =
                 fold { total = 0; signaled = 0; initial = 0 }
                 @@ fun { total; signaled; initial } one ->
                 let total = total + 1
                 and signaled = signaled + Bool.to_int (is_signaled one)
                 and initial = initial + Bool.to_int (not (is_signaled one)) in
                 { total; signaled; initial }
               in
               let before =
                 stats
                   (match !queue with
                   | Min1 (One r) -> One r
                   | Min1 (Many r) -> Many r)
               in
               let after = stats (cleanup !queue ~count) in
               assert (after.initial = before.initial);
               assert (after.signaled <= before.signaled);
               let last_bit = 1 lsl (n - 1) in
               let last_kept =
                 bits land last_bit = 0
                 && 1 <> n
                 && bits land lnot last_bit <> 0
               in
               if count = 1 then begin
                 assert (
                   after.signaled
                   <= before.signaled
                      - Bool.to_int (0 < before.signaled)
                      + Bool.to_int last_kept)
               end
               else begin
                 assert (after.total = after.initial + Bool.to_int last_kept)
               end
             done
           done
      end

    let signal awaiters ~count =
      let count = ref count in
      match awaiters with
      | Min1 (One one_r) -> signal_last_one (One one_r)
      | Min1 (Many many_r as many) ->
          exec many;
          let tail = many_r.tail in
          let head = ref tail in
          let work = ref many_r.head in
          while !work != tail do
            if is_signaled !work then work := next_of !work ~tail
            else if is_signalable !work then begin
              signal_and_clear !work;
              let next = next_of !work ~tail in
              let n = !count - 1 in
              count := n;
              if n <> 0 then work := next
              else begin
                head := next;
                work := tail
              end
            end
            else begin
              head := !work;
              work := tail
            end
          done;
          let head = !head in
          if head == tail then
            if !count <> 0 then signal_last_one head else last head
          else begin
            if !count <> 0 then begin
              let prev = ref head in
              let work = ref (next_of !prev ~tail) in
              while !work != tail do
                let next = next_of !work ~tail in
                if is_signaled !work then begin
                  set_next_of !prev next;
                  work := next
                end
                else if is_signalable !work then begin
                  signal_and_clear !work;
                  set_next_of !prev next;
                  let n = !count - 1 in
                  count := n;
                  if n <> 0 then work := next else work := tail
                end
                else begin
                  prev := !work;
                  work := next
                end
              done;
              if !count <> 0 && is_signalable tail then signal_and_clear tail
            end;
            Many { head; prev = tail; tail }
          end

    let ( (* test signal *) ) =
      if false then begin
        [ 1; Int.max_int ]
        |> List.iter @@ fun count ->
           for n = 1 to 4 do
             for signaled_bits = 0 to (1 lsl n) - 1 do
               for signalable_bits = 0 to (1 lsl n) - 1 do
                 let make i =
                   let trigger = Trigger.create () in
                   let value = signalable_bits land (1 lsl i) in
                   let awaitable = make 0 and next = Min0 Zero in
                   if signaled_bits land (1 lsl i) = 0 then
                     Trigger.signal trigger;
                   One { awaitable; value; trigger; counter = 0; next }
                 in
                 let queue = ref (Min1 (make 0)) in
                 for i = 1 to n - 1 do
                   queue := Min1 (snoc !queue (make i))
                 done;
                 let rec fold zero fn = function
                   | Zero -> zero
                   | One r -> fn zero (One r)
                   | Many { head; tail; _ } as many ->
                       exec many;
                       fn
                         (let head = next_of head ~tail in
                          if head != tail then
                            fold zero fn (Many { head; prev = tail; tail })
                          else fold zero fn (generalize head))
                         head
                 in
                 let open struct
                   type t = {
                     total : int;
                     signalable : int;
                     signaled : int;
                     initial : int;
                   }
                 end in
                 let stats =
                   fold { total = 0; signalable = 0; signaled = 0; initial = 0 }
                   @@ fun { total; signalable; signaled; initial } one ->
                   let total = total + 1
                   and signalable =
                     signalable
                     + Bool.to_int ((not (is_signaled one)) && is_signalable one)
                   and signaled = signaled + Bool.to_int (is_signaled one)
                   and initial =
                     initial + Bool.to_int (not (is_signaled one))
                   in
                   { total; signalable; signaled; initial }
                 in
                 let before =
                   stats
                     (match !queue with
                     | Min1 (One r) -> One r
                     | Min1 (Many r) -> Many r)
                 in
                 let after = stats (signal !queue ~count) in
                 (* The assertions here could be made stricter such that enough
                    signaled awaiters should be removed from the queue.  Please
                    make the assertions stricter if you plan to modify the
                    signaling logic! *)
                 if count = 1 then begin
                   assert (
                     (before.signalable = 0 && after.signalable = 0)
                     || after.signalable + 1 = before.signalable)
                 end
                 else begin
                   assert (after.signalable = 0)
                 end
               done
             done
           done
      end
  end

  module Htbl = Picos_aux_htbl

  let awaiters = Htbl.create ~hashed_type:(module Packed) ()

  let update t ~signal ~count =
    try
      let signal = ref signal in
      let count = ref count in
      let backoff = ref Backoff.default in
      while
        not
          (let before = Htbl.find_exn awaiters t in
           match
             if !signal then Awaiters.signal before ~count:!count
             else Awaiters.cleanup before ~count:!count
           with
           | Zero -> Htbl.try_compare_and_remove awaiters t before
           | One r ->
               let after = Awaiters.Min1 (One r) in
               before == after
               || Htbl.try_compare_and_set awaiters t before after
           | Many r ->
               let after = Awaiters.Min1 (Many r) in
               before == after
               || Htbl.try_compare_and_set awaiters t before after)
      do
        (* Even if the hash table update after signal fails, the trigger(s) have
           been signaled. *)
        signal := false;
        (* If a single awaiter and multi awaiter cleanup are attempted in
           parallel it might be that a multi awaiter cleanup "succeeds" and yet
           some awaiters are left in the queue.  For this reason we perform a
           multi awaiter cleanup after failure.  It might be possible to improve
           upon this with some more clever approach. *)
        count := Int.max_int;
        backoff := Backoff.once !backoff
      done
    with Not_found -> ()

  module Awaiter = struct
    type t = Awaiters.is1

    let add_as (type a) (t : a awaitable) trigger value =
      let one : Awaiters.is1 =
        One { awaitable = t; value; trigger; counter = 0; next = Min0 Zero }
      in
      let backoff = ref Backoff.default in
      while
        not
          (match Htbl.find_exn awaiters (Packed t) with
          | before ->
              let many = Awaiters.snoc before one in
              Htbl.try_compare_and_set awaiters (Packed t) before (Min1 many)
          | exception Not_found -> Htbl.try_add awaiters (Packed t) (Min1 one))
      do
        backoff := Backoff.once !backoff
      done;
      one

    let add (type a) (t : a awaitable) trigger =
      let unique_value = Sys.opaque_identity (Obj.magic awaiters : a) in
      add_as t trigger unique_value

    let remove one =
      Awaiters.signal_and_clear one;
      update (Awaiters.awaitable_of one) ~signal:false ~count:1
  end

  let await t value =
    let trigger = Trigger.create () in
    let one = Awaiter.add_as t trigger value in
    if Awaiters.is_signalable one then Awaiter.remove one
    else
      match Awaiters.await one with
      | None -> ()
      | Some exn_bt ->
          Awaiters.clear one;
          update (Awaiters.awaitable_of one) ~signal:true ~count:1;
          Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)

  let[@inline] broadcast t = update (Packed t) ~signal:true ~count:Int.max_int
  let[@inline] signal t = update (Packed t) ~signal:true ~count:1

  let () =
    Stdlib.at_exit @@ fun () ->
    match Htbl.find_random_exn awaiters with
    | _ ->
        (* This should not normally happen, but might happen due to the program
           being forced to exit without proper cleanup.  Otherwise this may
           indicate a bug in the cleanup of awaiters. *)
        Printf.eprintf "Awaitable leaked\n%!"
    | exception Not_found -> ()
end
