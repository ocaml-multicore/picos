module Atomic = Multicore_magic.Transparent_atomic

type 'a t = { head : 'a head Atomic.t; tail : 'a tail Atomic.t }

and ('a, _) tdt =
  | Cons : {
      counter : int;
      value : 'a;
      suffix : 'a head;
    }
      -> ('a, [> `Cons ]) tdt
  | Head : { counter : int } -> ('a, [> `Head ]) tdt
  | Snoc : {
      counter : int;
      prefix : 'a tail;
      value : 'a;
    }
      -> ('a, [> `Snoc ]) tdt
  | Tail : {
      counter : int;
      mutable move : ('a, [ `Snoc | `Used ]) tdt;
    }
      -> ('a, [> `Tail ]) tdt
  | Used : ('a, [> `Used ]) tdt

and 'a head = H : ('a, [< `Cons | `Head ]) tdt -> 'a head [@@unboxed]
and 'a tail = T : ('a, [< `Snoc | `Tail ]) tdt -> 'a tail [@@unboxed]

let create ?padded () =
  let head =
    Atomic.make (H (Head { counter = 1 })) |> Multicore_magic.copy_as ?padded
  in
  let tail =
    Atomic.make (T (Tail { counter = 0; move = Obj.magic () }))
    |> Multicore_magic.copy_as ?padded
  in
  Multicore_magic.copy_as ?padded { head; tail }

let rec rev (suffix : (_, [< `Cons ]) tdt) = function
  | T (Snoc { counter; prefix; value }) ->
      rev (Cons { counter; value; suffix = H suffix }) prefix
  | T (Tail _) -> suffix

let rev = function
  | (Snoc { counter; prefix; value } : (_, [< `Snoc ]) tdt) ->
      rev
        (Cons { counter; value; suffix = H (Head { counter = counter + 1 }) })
        prefix

let rec push t value backoff = function
  | T (Snoc snoc_r as snoc) -> push_with t value backoff snoc_r.counter (T snoc)
  | T (Tail tail_r as tail) -> begin
      match tail_r.move with
      | Used -> push_with t value backoff tail_r.counter (T tail)
      | Snoc move_r as move ->
          begin
            match Atomic.get t.head with
            | H (Head head_r as head) when head_r.counter < move_r.counter ->
                let after = rev move in
                if
                  Atomic.fenceless_get t.head == H head
                  && Atomic.compare_and_set t.head (H head) (H after)
                then tail_r.move <- Used
            | _ -> ()
          end;
          let new_tail = Atomic.fenceless_get t.tail in
          if new_tail != T tail then push t value backoff new_tail
          else push_with t value backoff tail_r.counter (T tail)
    end

and push_with t value backoff counter prefix =
  let after = Snoc { counter = counter + 1; prefix; value } in
  let new_tail = Atomic.fenceless_get t.tail in
  if new_tail != prefix then push t value backoff new_tail
  else if not (Atomic.compare_and_set t.tail prefix (T after)) then
    let backoff = Backoff.once backoff in
    push t value backoff (Atomic.fenceless_get t.tail)

let push t value = push t value Backoff.default (Atomic.fenceless_get t.tail)

let rec push_head t value backoff =
  match Atomic.get t.head with
  | H (Cons cons_r) as suffix ->
      let after = Cons { counter = cons_r.counter - 1; value; suffix } in
      if not (Atomic.compare_and_set t.head suffix (H after)) then
        push_head t value (Backoff.once backoff)
  | H (Head head_r) as head -> begin
      match Atomic.get t.tail with
      | T (Snoc snoc_r as move) ->
          if Atomic.get t.head != head then push_head t value backoff
          else if head_r.counter = snoc_r.counter then begin
            let prefix = T (Snoc { snoc_r with value }) in
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
      | T (Tail tail_r) as prefix -> begin
          match tail_r.move with
          | Used ->
              if Atomic.get t.head == head then begin
                let tail =
                  Snoc { counter = tail_r.counter + 1; value; prefix }
                in
                if not (Atomic.compare_and_set t.tail prefix (T tail)) then
                  push_head t value (Backoff.once backoff)
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
                | _ -> ()
              end;
              push_head t value backoff
        end
    end

let push_head t value = push_head t value Backoff.default

exception Empty

let rec pop t backoff = function
  | H (Cons cons_r as cons) ->
      if Atomic.compare_and_set t.head (H cons) cons_r.suffix then cons_r.value
      else
        let backoff = Backoff.once backoff in
        pop t backoff (Atomic.fenceless_get t.head)
  | H (Head head_r as head) -> begin
      match Atomic.fenceless_get t.tail with
      | T (Snoc snoc_r as move) ->
          if head_r.counter = snoc_r.counter then
            if Atomic.compare_and_set t.tail (T move) snoc_r.prefix then
              snoc_r.value
            else pop t backoff (Atomic.fenceless_get t.head)
          else
            let tail = Tail { counter = snoc_r.counter; move } in
            let new_head = Atomic.get t.head in
            if new_head != H head then pop t backoff new_head
            else if Atomic.compare_and_set t.tail (T move) (T tail) then
              pop_moving t backoff head move tail
            else pop t backoff (Atomic.fenceless_get t.head)
      | T (Tail tail_r as tail) -> begin
          match tail_r.move with
          | Used -> pop_emptyish t backoff head
          | Snoc _ as move -> pop_moving t backoff head move tail
        end
    end

and pop_moving t backoff (Head head_r as head : (_, [ `Head ]) tdt)
    (Snoc move_r as move : (_, [ `Snoc ]) tdt)
    (Tail tail_r : (_, [ `Tail ]) tdt) =
  if head_r.counter < move_r.counter then
    match rev move with
    | Cons cons_r ->
        let after = cons_r.suffix in
        let new_head = Atomic.get t.head in
        if new_head != H head then pop t backoff new_head
        else if Atomic.compare_and_set t.head (H head) after then begin
          tail_r.move <- Used;
          cons_r.value
        end
        else
          let backoff = Backoff.once backoff in
          pop t backoff (Atomic.fenceless_get t.head)
  else pop_emptyish t backoff head

and pop_emptyish t backoff head =
  let new_head = Atomic.get t.head in
  if new_head == H head then raise_notrace Empty else pop t backoff new_head

let pop_exn t = pop t Backoff.default (Atomic.fenceless_get t.head)
