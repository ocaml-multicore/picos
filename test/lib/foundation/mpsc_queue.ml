type 'a t = { tail : 'a tail Atomic.t; head : 'a head ref }

and ('a, _) tdt =
  | Head : ('a, [> `Head ]) tdt
  | Cons : { value : 'a; next : 'a head } -> ('a, [> `Cons ]) tdt
  | Tail : ('a, [> `Tail ]) tdt
  | Snoc : { mutable prev : 'a tail; value : 'a } -> ('a, [> `Snoc ]) tdt

and 'a head = H : ('a, [< `Head | `Cons ]) tdt -> 'a head [@@unboxed]
and 'a tail = T : ('a, [< `Tail | `Snoc ]) tdt -> 'a tail [@@unboxed]

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make (T Tail) in
  let head = Multicore_magic.copy_as_padded @@ ref (H Head) in
  Multicore_magic.copy_as_padded { tail; head }

let rec enqueue tail (Snoc r as after : (_, [< `Snoc ]) tdt) backoff =
  let before = Atomic.get tail in
  r.prev <- before;
  if not (Atomic.compare_and_set tail before (T after)) then
    enqueue tail after (Backoff.once backoff)

let enqueue t value =
  let tail = t.tail in
  let before = Atomic.get tail in
  let after = Snoc { prev = before; value } in
  if not (Atomic.compare_and_set tail before (T after)) then
    enqueue tail after Backoff.default

exception Empty

let rec rev_to head (Snoc r : (_, [< `Snoc ]) tdt) =
  let head = Cons { value = r.value; next = H head } in
  match r.prev with T Tail -> head | T (Snoc _ as prev) -> rev_to head prev

let dequeue t =
  match !(t.head) with
  | H (Cons r) ->
      t.head := r.next;
      r.value
  | H Head ->
      if Atomic.get t.tail == T Tail then raise_notrace Empty
      else begin
        match Atomic.exchange t.tail (T Tail) with
        | T Tail ->
            (* Impossible *)
            raise_notrace Empty
        | T (Snoc r) -> begin
            match r.prev with
            | T Tail -> r.value
            | T (Snoc _ as prev) ->
                let (Cons r : (_, [< `Cons ]) tdt) =
                  rev_to (Cons { value = r.value; next = H Head }) prev
                in
                t.head := r.next;
                r.value
          end
      end
