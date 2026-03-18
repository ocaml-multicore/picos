open Picos
module Atomic = Multicore_magic.Transparent_atomic

type 'a t = { head : 'a head Atomic.t; tail : 'a tail Atomic.t }

and ('a, _) tdt =
  | Cons : {
      counter : int;
      value : 'a;
      suffix : ('a, [ `Cons | `Head ]) tdt;
    }
      -> ('a, [> `Cons ]) tdt
  | Head : { counter : int } -> ('a, [> `Head ]) tdt
  | Snoc : {
      counter : int;
      prefix : ('a, [ `Snoc | `Tail ]) tdt;
      value : 'a;
    }
      -> ('a, [> `Snoc ]) tdt
  | Tail : {
      counter : int;
      mutable move : ('a, [ `Snoc | `Used | `Wait ]) tdt;
    }
      -> ('a, [> `Tail ]) tdt
  | Used : ('a, [> `Used ]) tdt
  | Wait : {
      trigger : Trigger.t;
      next : ('a, [ `Wait | `Used ]) tdt;
    }
      -> ('a, [> `Wait ]) tdt

and 'a head = H : ('a, [< `Cons | `Head ]) tdt -> 'a head [@@unboxed]
and 'a tail = T : ('a, [< `Snoc | `Tail ]) tdt -> 'a tail [@@unboxed]

let create ?padded () =
  let head =
    Atomic.make (H (Head { counter = 1 })) |> Multicore_magic.copy_as ?padded
  in
  let tail =
    Atomic.make (T (Tail { counter = 0; move = Used }))
    |> Multicore_magic.copy_as ?padded
  in
  Multicore_magic.copy_as ?padded { head; tail }

let rec signal = function
  | Used -> ()
  | Wait wait_r ->
      Trigger.signal wait_r.trigger;
      signal wait_r.next

let[@inline] signal (Tail tail_r : (_, [ `Tail ]) tdt) =
  match tail_r.move with
  | Used | Snoc _ -> ()
  | Wait _ as wait ->
      tail_r.move <- Used;
      signal wait

let rec rev (Cons _ as suffix : (_, [< `Cons ]) tdt) = function
  | Snoc { counter; prefix; value } ->
      rev (Cons { counter; value; suffix }) prefix
  | Tail _ -> suffix

let rev : _ -> (_, [ `Cons ]) tdt = function
  | (Snoc { counter; prefix; value } : (_, [< `Snoc ]) tdt) ->
      rev
        (Cons { counter; value; suffix = Head { counter = counter + 1 } })
        prefix

let rec push t value backoff = function
  | T (Snoc snoc_r as prefix) ->
      let after = Snoc { counter = snoc_r.counter + 1; prefix; value } in
      if not (Atomic.compare_and_set t.tail (T prefix) (T after)) then
        let backoff = Backoff.once backoff in
        push t value backoff (Atomic.fenceless_get t.tail)
  | T (Tail tail_r as prefix) -> begin
      match tail_r.move with
      | Used | Wait _ ->
          let after = Snoc { counter = tail_r.counter + 1; prefix; value } in
          if Atomic.compare_and_set t.tail (T prefix) (T after) then
            signal prefix
          else
            let backoff = Backoff.once backoff in
            push t value backoff (Atomic.fenceless_get t.tail)
      | Snoc move_r as move ->
          begin
            match Atomic.get t.head with
            | H (Head head_r as head) when head_r.counter < move_r.counter ->
                let after = rev move in
                if
                  Atomic.fenceless_get t.head == H head
                  && Atomic.compare_and_set t.head (H head) (H after)
                then tail_r.move <- Used
            | _ -> tail_r.move <- Used
          end;
          push t value backoff (Atomic.get t.tail)
    end

exception Empty

type ('a, _) on_empty =
  | Return_None : ('a, 'a option) on_empty
  | Raise_Empty : ('a, 'a) on_empty
  | Await : ('a, 'a) on_empty

let rec pop_as : type a r. a t -> (a, r) on_empty -> _ -> a head -> r =
 fun t on_empty backoff -> function
  | H (Cons cons_r as cons) ->
      if Atomic.compare_and_set t.head (H cons) (H cons_r.suffix) then
        match on_empty with
        | Return_None -> Some cons_r.value
        | Raise_Empty -> cons_r.value
        | Await -> cons_r.value
      else
        let backoff = Backoff.once backoff in
        pop_as t on_empty backoff (Atomic.fenceless_get t.head)
  | H (Head head_r as head) -> begin
      match Atomic.get t.tail with
      | T (Snoc snoc_r as move) ->
          if head_r.counter = snoc_r.counter then
            if Atomic.compare_and_set t.tail (T move) (T snoc_r.prefix) then
              match on_empty with
              | Return_None -> Some snoc_r.value
              | Raise_Empty -> snoc_r.value
              | Await -> snoc_r.value
            else pop_as t on_empty backoff (Atomic.fenceless_get t.head)
          else
            let (Tail tail_r as tail : (_, [ `Tail ]) tdt) =
              Tail { counter = snoc_r.counter; move }
            in
            let new_head = Atomic.get t.head in
            if new_head != H head then pop_as t on_empty backoff new_head
            else if Atomic.compare_and_set t.tail (T move) (T tail) then
              let (Cons cons_r) = rev move in
              let after = cons_r.suffix in
              let new_head = Atomic.get t.head in
              if new_head != H head then pop_as t on_empty backoff new_head
              else if Atomic.compare_and_set t.head (H head) (H after) then begin
                tail_r.move <- Used;
                match on_empty with
                | Return_None -> Some cons_r.value
                | Raise_Empty -> cons_r.value
                | Await -> cons_r.value
              end
              else
                let backoff = Backoff.once backoff in
                pop_as t on_empty backoff (Atomic.fenceless_get t.head)
            else pop_as t on_empty backoff (Atomic.fenceless_get t.head)
      | T (Tail tail_r as tail) -> begin
          match tail_r.move with
          | (Used | Wait _) as next -> begin
              let new_head = Atomic.get t.head in
              if new_head != H head then pop_as t on_empty backoff new_head
              else
                match on_empty with
                | Return_None -> None
                | Raise_Empty -> raise_notrace Empty
                | Await -> await t on_empty backoff tail tail_r.counter next
            end
          | Snoc move_r as move -> begin
              if head_r.counter < move_r.counter then
                let (Cons cons_r) = rev move in
                let after = cons_r.suffix in
                let new_head = Atomic.get t.head in
                if new_head != H head then pop_as t on_empty backoff new_head
                else if Atomic.compare_and_set t.head (H head) (H after) then begin
                  tail_r.move <- Used;
                  match on_empty with
                  | Return_None -> Some cons_r.value
                  | Raise_Empty -> cons_r.value
                  | Await -> cons_r.value
                end
                else
                  let backoff = Backoff.once backoff in
                  pop_as t on_empty backoff (Atomic.fenceless_get t.head)
              else
                let new_head = Atomic.get t.head in
                if new_head != H head then pop_as t on_empty backoff new_head
                else
                  match on_empty with
                  | Return_None -> None
                  | Raise_Empty -> raise_notrace Empty
                  | Await -> await t on_empty backoff tail tail_r.counter Used
            end
        end
    end

and await : type a r.
    a t -> (a, r) on_empty -> _ -> (a, _) tdt -> int -> (a, _) tdt -> r =
 fun t on_empty backoff tail counter next ->
  let trigger = Trigger.create () in
  let (Wait wait_r as move : (_, [ `Wait ]) tdt) = Wait { trigger; next } in
  let after = Tail { counter; move } in
  if Atomic.compare_and_set t.tail (T tail) (T after) then
    match Trigger.await wait_r.trigger with
    | None -> pop_as t on_empty Backoff.default (Atomic.fenceless_get t.head)
    | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt (* TODO *)
  else
    let backoff = Backoff.once backoff in
    pop_as t on_empty backoff (Atomic.fenceless_get t.head)

let rec push_head t value backoff =
  match Atomic.get t.head with
  | H (Cons cons_r as suffix) ->
      let after = Cons { counter = cons_r.counter - 1; value; suffix } in
      if not (Atomic.compare_and_set t.head (H suffix) (H after)) then
        push_head t value (Backoff.once backoff)
  | H (Head head_r as head) -> begin
      match Atomic.get t.tail with
      | T (Snoc snoc_r as move) ->
          if Atomic.get t.head != H head then push_head t value backoff
          else if head_r.counter = snoc_r.counter then begin
            let prefix = Snoc { snoc_r with value } in
            let after =
              Snoc { snoc_r with counter = snoc_r.counter + 1; prefix }
            in
            if not (Atomic.compare_and_set t.tail (T move) (T after)) then
              push_head t value (Backoff.once backoff)
          end
          else
            let tail = Tail { counter = snoc_r.counter; move } in
            let backoff =
              if Atomic.compare_and_set t.tail (T move) (T tail) then backoff
              else Backoff.once backoff
            in
            push_head t value backoff
      | T (Tail tail_r as prefix) -> begin
          match tail_r.move with
          | Used | Wait _ ->
              if Atomic.get t.head == H head then begin
                let tail =
                  Snoc { counter = tail_r.counter + 1; value; prefix }
                in
                if Atomic.compare_and_set t.tail (T prefix) (T tail) then
                  signal prefix
                else push_head t value (Backoff.once backoff)
              end
              else push_head t value backoff
          | Snoc move_r as move ->
              begin
                match Atomic.get t.head with
                | H (Head head_r as head) when head_r.counter < move_r.counter
                  ->
                    let after = rev move in
                    if
                      Atomic.fenceless_get t.head == H head
                      && Atomic.compare_and_set t.head (H head) (H after)
                    then tail_r.move <- Used
                | _ -> tail_r.move <- Used
              end;
              push_head t value backoff
        end
    end

let rec length t =
  let head = Atomic.get t.head in
  let tail = Atomic.fenceless_get t.tail in
  if head != Atomic.get t.head then length t
  else
    let head_at =
      match head with H (Cons r) -> r.counter | H (Head r) -> r.counter
    in
    let tail_at =
      match tail with T (Snoc r) -> r.counter | T (Tail r) -> r.counter
    in
    tail_at - head_at + 1

let[@inline] is_empty t = length t == 0

let[@inline] pop_exn t =
  pop_as t Raise_Empty Backoff.default (Atomic.fenceless_get t.head)

let[@inline] pop_opt t =
  pop_as t Return_None Backoff.default (Atomic.fenceless_get t.head)

let[@inline] pop t =
  pop_as t Await Backoff.default (Atomic.fenceless_get t.head)

let[@inline] push t value =
  push t value Backoff.default (Atomic.fenceless_get t.tail)

let[@inline] push_head t value = push_head t value Backoff.default
