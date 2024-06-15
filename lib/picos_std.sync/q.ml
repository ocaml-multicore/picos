type ('a, _) queue =
  | Zero : ('a, [> `Zero ]) queue
  | One : {
      head : 'a S.cons;
      tail : 'a S.cons;
      cons : 'a S.cons;
    }
      -> ('a, [> `One ]) queue

type ('a, 'n) one = ('a, ([< `One ] as 'n)) queue
type 'a t = T : ('a, [< `Zero | `One ]) queue -> 'a t [@@unboxed]

let[@inline] singleton value =
  let cons = S.Cons { value; next = T Nil } in
  T (One { head = cons; tail = cons; cons })

let[@inline] exec (One o : (_, _) one) =
  if o.tail != o.cons then
    let (Cons tl) = o.tail in
    if tl.next != T o.cons then tl.next <- T o.cons

let[@inline] snoc (One o as t : (_, _) one) value =
  exec t;
  let cons = S.Cons { value; next = T Nil } in
  T (One { head = o.head; tail = o.cons; cons })

let[@inline] add t value =
  match t with T Zero -> singleton value | T (One _ as o) -> snoc o value

let[@inline] add_cons t cons =
  match t with
  | T Zero -> T (One { head = cons; tail = cons; cons })
  | T (One r as o) ->
      exec o;
      T (One { head = r.head; tail = r.cons; cons })

let[@inline] head (One { head = Cons hd; _ } : (_, _) one) = hd.value

let[@inline] tail (One o as t : (_, _) one) =
  exec t;
  if o.head == o.cons then T Zero
  else
    let (Cons hd) = o.head in
    T (One { head = S.as_cons hd.next; tail = o.cons; cons = o.cons })

let[@inline] iter action (One o as t : (_, _) one) =
  exec t;
  S.iter action o.head o.cons

let remove (One o as t : (_, _) one) value =
  exec t;
  match S.reject o.head value with
  | S.T Nil -> T Zero
  | S.T (Cons _ as head) ->
      let tail = S.find_tail head in
      T (One { head; tail; cons = tail })
  | exception Not_found -> T t

let reverse_as_queue = function
  | S.T Nil -> T Zero
  | S.T (Cons cons_r as tail) ->
      let head = S.reverse_to tail cons_r.next in
      T (One { head; tail; cons = tail })
