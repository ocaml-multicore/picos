open Picos

module Q = struct
  type ('a, _) node =
    | Nil : ('a, [> `Nil ]) node
    | Cons : {
        mutable next : 'a next;
        mutable value : 'a;
      }
        -> ('a, [> `Cons ]) node

  and 'a next = N : ('a, [< `Nil | `Cons ]) node -> 'a next [@@unboxed]

  external as_atomic : ('a, [< `Cons ]) node -> 'a next Atomic.t = "%identity"

  type 'a cons = ('a, [ `Cons ]) node
  type 'a t = { head : 'a cons Atomic.t; tail : 'a cons Atomic.t }

  let create ?padded () =
    let node : _ cons = Cons { next = N Nil; value = Obj.magic () } in
    let head = Atomic.make node |> Multicore_magic.copy_as ?padded in
    let tail = Atomic.make node |> Multicore_magic.copy_as ?padded in
    Multicore_magic.copy_as ?padded { head; tail }

  (* *)

  let rec fix_tail tail new_tail backoff =
    let old_tail = Atomic.get tail in
    if
      Atomic.get (as_atomic new_tail) == N Nil
      && not (Atomic.compare_and_set tail old_tail new_tail)
    then fix_tail tail new_tail (Backoff.once backoff)

  let rec add tail link (new_node : _ cons) backoff =
    match Atomic.get (as_atomic link) with
    | N Nil ->
        if Atomic.compare_and_set (as_atomic link) (N Nil) (N new_node) then begin
          fix_tail tail new_node Backoff.default;
          link
        end
        else
          let backoff = Backoff.once backoff in
          add tail link new_node backoff
    | N (Cons _ as cons) -> add tail cons new_node backoff

  let create_node value : _ cons = Cons { next = N Nil; value }

  let add t (new_node : _ cons) : 'a cons =
    let old_tail = Atomic.get t.tail in
    if Atomic.compare_and_set (as_atomic old_tail) (N Nil) (N new_node) then begin
      (* If the below CAS fails a new node was inserted and we are done. *)
      Atomic.compare_and_set t.tail old_tail new_node |> ignore;
      old_tail
    end
    else
      let backoff = Backoff.once Backoff.default in
      add t.tail old_tail new_node backoff

  (* *)

  let all t = Atomic.get t.head
  let next cons = Atomic.get (as_atomic cons)

  let remove t (prev : 'a cons) (Cons curr_r as curr : 'a cons) null =
    match Atomic.get (as_atomic curr) with
    | N Nil ->
        (* [curr] is the last node, we must clear it *)
        curr_r.value <- null;
        if Atomic.get t.head == prev then
          Atomic.compare_and_set t.head prev curr |> ignore
    | N (Cons _) as next ->
        (* If below CAS fails, someone else must have removed [curr] *)
        Atomic.compare_and_set (as_atomic prev) (N curr) next |> ignore
end

external unit_as_false : unit -> bool = "%identity"

module Tx = Computation.Tx

type 'a taker =
  | T : {
      computation : (unit -> 'r) Computation.t;
      result : 'a -> 'r;
    }
      -> 'a taker

type 'a giver =
  | G : {
      computation : (unit -> 'r) Computation.t;
      result : unit -> 'r;
      value : 'a;
    }
      -> 'a giver

let null_taker = T { computation = Computation.exited (); result = Fun.id }

and null_giver =
  G { computation = Computation.exited (); result = Fun.id; value = () }

external generalize_taker : unit taker -> 'a taker = "%identity"
external generalize_giver : unit giver -> 'a giver = "%identity"

type 'a t = { givers : 'a giver Q.t; takers : 'a taker Q.t }

let create ?padded () =
  let givers = Q.create ?padded () and takers = Q.create ?padded () in
  Multicore_magic.copy_as ?padded { givers; takers }

(* *)

let[@inline never] wait computation =
  let trigger = Trigger.create () in
  if Computation.try_attach computation trigger then
    match Trigger.await trigger with
    | None -> ()
    | Some exn_bt ->
        if Computation.try_cancel computation (Exn_bt.get_callstack 0 Exit) then
          Exn_bt.raise exn_bt

(* *)

let rec give_as t (G gr as self) prev =
  match Q.next prev with
  | N Nil -> false
  | N (Cons { value = T tr; _ } as curr) ->
      if Tx.same tr.computation gr.computation then give_as t self curr
      else
        let tx = Tx.create () in
        let result = tr.result in
        let value = gr.value in
        if not (Tx.try_return tx tr.computation (fun () -> result value)) then begin
          Q.remove t.takers prev curr (generalize_taker null_taker);
          give_as t self curr
        end
        else if
          (not (Tx.try_return tx gr.computation gr.result))
          || not (Tx.try_commit tx)
        then
          (not (Computation.is_running gr.computation))
          || give_as t self
               (if Computation.is_running tr.computation then prev else curr)
        else begin
          Q.remove t.takers prev curr (generalize_taker null_taker);
          true
        end

let rec give t value prev =
  match Q.next prev with
  | N Nil ->
      let computation = Computation.create () in
      let self = G { computation; result = Fun.id; value } in
      let curr = Q.create_node self in
      let prev = Q.add t.givers curr in
      if not (give_as t self (Q.all t.takers)) then wait computation;
      Q.remove t.givers prev curr (generalize_giver null_giver)
  | N (Cons { value = T { computation; result }; _ } as curr) ->
      let got = Computation.try_return computation (fun () -> result value) in
      Q.remove t.takers prev curr (generalize_taker null_taker);
      if not got then give t value curr

let give_evt t value =
  let request computation result =
    let self = G { computation; result; value } in
    let curr = Q.create_node self in
    let prev = Q.add t.givers curr in
    if give_as t self (Q.all t.takers) then
      Q.remove t.givers prev curr (generalize_giver null_giver)
  in
  Event.from_request { request }

(* *)

let rec take_as t (T tr as self) prev =
  match Q.next prev with
  | N Nil -> false
  | N (Cons { value = G gr; _ } as curr) ->
      if Tx.same tr.computation gr.computation then take_as t self curr
      else
        let tx = Tx.create () in
        let result = tr.result in
        let value = gr.value in
        if not (Tx.try_return tx gr.computation gr.result) then begin
          Q.remove t.givers prev curr (generalize_giver null_giver);
          take_as t self curr
        end
        else if
          (not (Tx.try_return tx tr.computation (fun () -> result value)))
          || not (Tx.try_commit tx)
        then
          (not (Computation.is_running tr.computation))
          || take_as t self
               (if Computation.is_running gr.computation then prev else curr)
        else begin
          Q.remove t.givers prev curr (generalize_giver null_giver);
          true
        end

let rec take t prev =
  match Q.next prev with
  | N Nil ->
      let computation = Computation.create () in
      let self = T { computation; result = Fun.id } in
      let curr = Q.create_node self in
      let prev = Q.add t.takers curr in
      if not (take_as t self (Q.all t.givers)) then wait computation;
      Q.remove t.takers prev curr (generalize_taker null_taker);
      Computation.await computation ()
  | N (Cons { value = G { computation; result; value }; _ } as curr) ->
      let got = Computation.try_return computation result in
      Q.remove t.givers prev curr (generalize_giver null_giver);
      if got then value else take t curr

let take_evt t =
  let request computation result =
    let self = T { computation; result } in
    let curr = Q.create_node self in
    let prev = Q.add t.takers curr in
    if take_as t self (Q.all t.givers) then
      Q.remove t.takers prev curr (generalize_taker null_taker)
  in
  Event.from_request { request }

(* *)

let[@inline] give t value = give t value (Q.all t.takers)
let[@inline] take t = take t (Q.all t.givers)
