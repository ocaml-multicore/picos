type 'a t = { mutable value : 'a; bits : int }

external as_atomic : 'a t -> 'a Atomic.t = "%identity"

let make ?padded value =
  Multicore_magic.copy_as ?padded { value; bits = Random.bits () }

let[@inline] get t = Atomic.get (Sys.opaque_identity (as_atomic t))
let[@inline] compare_and_set x b a = Atomic.compare_and_set (as_atomic x) b a
let[@inline] exchange t value = Atomic.exchange (as_atomic t) value
let[@inline] fetch_and_add t n = Atomic.fetch_and_add (as_atomic t) n
let[@inline] set t value = exchange t value |> ignore
let[@inline] incr t = fetch_and_add t 1 |> ignore
let[@inline] decr t = fetch_and_add t (-1) |> ignore

(* *)

open Picos
module Htbl = Picos_aux_htbl

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
        awaitable : 'a awaitable;
        value : 'a;
        trigger : Trigger.t;
        mutable next : min0;
      }
        -> [> `One ] tdt
    | Many : { head : is1; tail : is1; snoc : is1 } -> [> `Many ] tdt

  and min0 = Min0 : [< `Zero | `One ] tdt -> min0 [@@unboxed]
  and min1 = Min1 : [< `One | `Many ] tdt -> min1 [@@unboxed]
  and is1 = [ `One ] tdt

  let[@inline] exec (Many r : [ `Many ] tdt) =
    if r.tail != r.snoc then
      let (One tail_r) = r.tail in
      if tail_r.next != Min0 r.snoc then tail_r.next <- Min0 r.snoc

  let[@inline] snoc t snoc =
    match t with
    | Min1 (One r) -> Many { head = One r; tail = One r; snoc }
    | Min1 (Many r as many) ->
        exec many;
        Many { head = r.head; tail = r.snoc; snoc }

  let signal = function
    | Min1 (One r) ->
        if get r.awaitable != r.value then begin
          Trigger.signal r.trigger;
          Zero
        end
        else if Trigger.is_signaled r.trigger then Zero
        else One r
    | Min1 (Many r as many) -> begin
        exec many;
        let[@tail_mod_cons] rec signal dropped = function
          | Min0 Zero -> if dropped then Min0 Zero else raise_notrace Not_found
          | Min0 (One r) ->
              if get r.awaitable != r.value then
                if Trigger.is_signaled r.trigger then signal true r.next
                else begin
                  Trigger.signal r.trigger;
                  r.next
                end
              else if Trigger.is_signaled r.trigger then signal true r.next
              else Min0 (One { r with next = signal dropped r.next })
        in
        match signal false (Min0 r.head) with
        | Min0 Zero -> Zero
        | Min0 (One head_r) ->
            if One head_r == r.snoc then One head_r
            else
              let rec find_tail (One one_r as one : is1) =
                match one_r.next with
                | Min0 Zero -> one
                | Min0 (One one_r) -> find_tail (One one_r)
              in
              let (One tail_r) = find_tail (One head_r) in
              if One head_r == One tail_r then One tail_r
              else
                Many { head = One head_r; tail = One tail_r; snoc = One tail_r }
        | exception Not_found -> Many r
      end
end

let awaiters : (Packed.t, Awaiters.min1) Htbl.t =
  Htbl.create ~hashed_type:(module Packed) ()

let cleanup () = ()

let signal t =
  match Htbl.find_exn awaiters (Packed t) with
  | before -> begin
      match Awaiters.signal before with
      | Zero ->
          if not (Htbl.try_compare_and_remove awaiters (Packed t) before) then
            cleanup ()
      | One r ->
          let after = Awaiters.Min1 (One r) in
          if
            before != after
            && not (Htbl.try_compare_and_set awaiters (Packed t) before after)
          then cleanup ()
      | Many r ->
          let after = Awaiters.Min1 (Many r) in
          if
            before != after
            && not (Htbl.try_compare_and_set awaiters (Packed t) before after)
          then cleanup ()
    end
  | exception Not_found -> ()

let rec add_awaiter t (one : Awaiters.is1) =
  match Htbl.find_exn awaiters (Packed t) with
  | before ->
      let many = Awaiters.snoc before one in
      if not (Htbl.try_compare_and_set awaiters (Packed t) before (Min1 many))
      then add_awaiter t one
  | exception Not_found ->
      if not (Htbl.try_add awaiters (Packed t) (Min1 one)) then
        add_awaiter t one

let await awaitable value =
  let trigger = Trigger.create () in
  add_awaiter awaitable (One { awaitable; value; trigger; next = Min0 Zero });
  if get awaitable != value then Trigger.signal trigger
  else
    match Trigger.await trigger with
    | None -> ()
    | Some exn_bt ->
        signal awaitable;
        Printexc.raise_with_backtrace (fst exn_bt) (snd exn_bt)
