type 'a t = { tail : 'a tail Atomic.t; head : 'a head Atomic.t }

and ('a, _) tdt =
  | Head : ('a, [> `Head ]) tdt
  | Cons : { value : 'a; mutable next : 'a head } -> ('a, [> `Cons ]) tdt
  | Tail : ('a, [> `Tail ]) tdt
  | Snoc : { mutable prev : 'a tail; value : 'a } -> ('a, [> `Snoc ]) tdt

and 'a head = H : ('a, [< `Head | `Cons ]) tdt -> 'a head [@@unboxed]
and 'a tail = T : ('a, [< `Tail | `Snoc ]) tdt -> 'a tail [@@unboxed]

exception Empty

let[@inline never] impossible () = invalid_arg "multiple consumers not allowed"

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make (T Tail) in
  let head = Multicore_magic.copy_as_padded @@ Atomic.make (H Head) in
  Multicore_magic.copy_as_padded { tail; head }

let rec push_head head (Cons r as after : (_, [< `Cons ]) tdt) backoff =
  let before = Atomic.get head in
  r.next <- before;
  if not (Atomic.compare_and_set head before (H after)) then
    push_head head after (Backoff.once backoff)

let push_head t value =
  let head = t.head in
  let before = Atomic.get head in
  let after = Cons { value; next = before } in
  if not (Atomic.compare_and_set head before (H after)) then
    push_head head after Backoff.default

let rec append_to (Cons cons_r : (_, [< `Cons ]) tdt) tail =
  match cons_r.next with
  | H Head -> cons_r.next <- tail
  | H (Cons _ as head) -> append_to head tail

let rec push tail (Snoc r as after : (_, [< `Snoc ]) tdt) backoff =
  let before = Atomic.get tail in
  r.prev <- before;
  if not (Atomic.compare_and_set tail before (T after)) then
    push tail after (Backoff.once backoff)

let push t value =
  let tail = t.tail in
  let before = Atomic.get tail in
  let after = Snoc { prev = before; value } in
  if not (Atomic.compare_and_set tail before (T after)) then
    push tail after Backoff.default

let rec rev_to head (Snoc r : (_, [< `Snoc ]) tdt) =
  let head = Cons { value = r.value; next = H head } in
  match r.prev with T Tail -> head | T (Snoc _ as prev) -> rev_to head prev

let rec pop_exn t backoff = function
  | H (Cons head_r as head) ->
      if Atomic.compare_and_set t.head (H head) head_r.next then head_r.value
      else
        let backoff = Backoff.once backoff in
        pop_exn t backoff (Atomic.get t.head)
  | H Head ->
      if Atomic.get t.tail != T Tail then
        match Atomic.exchange t.tail (T Tail) with
        | T Tail -> impossible ()
        | T (Snoc snoc_r) -> begin
            match snoc_r.prev with
            | T Tail -> begin
                match Atomic.get t.head with
                | H Head -> snoc_r.value
                | H (Cons _ as head) ->
                    let next = Cons { value = snoc_r.value; next = H Head } in
                    append_to head (H next);
                    pop_head_exn t backoff head
              end
            | T (Snoc _ as prev) ->
                let next = Cons { value = snoc_r.value; next = H Head } in
                let (Cons cons_r as next : (_, [< `Cons ]) tdt) =
                  rev_to next prev
                in
                if Atomic.compare_and_set t.head (H Head) cons_r.next then
                  cons_r.value
                else begin
                  match Atomic.get t.head with
                  | H Head -> impossible ()
                  | H (Cons _ as head) ->
                      append_to head (H next);
                      pop_head_exn t backoff head
                end
          end
      else begin
        match Atomic.get t.head with
        | H Head -> raise_notrace Empty
        | H (Cons _ as head) -> pop_head_exn t backoff head
      end

and pop_head_exn t backoff (Cons head_r as head : (_, [< `Cons ]) tdt) =
  if Atomic.compare_and_set t.head (H head) head_r.next then head_r.value
  else
    let backoff = Backoff.once backoff in
    pop_exn t backoff (Atomic.get t.head)

let[@inline] pop_exn t = pop_exn t Backoff.default (Atomic.get t.head)
