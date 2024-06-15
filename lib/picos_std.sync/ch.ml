open Picos_std_event
open Picos
module Atomic = Multicore_magic.Transparent_atomic
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

type 'a state = { givers : 'a giver Q.t; takers : 'a taker Q.t }

let empty = { givers = T Zero; takers = T Zero }

type 'a t = 'a state Atomic.t

let create ?padded () = Atomic.make empty |> Multicore_magic.copy_as ?padded

(* *)

let[@inline never] wait computation =
  let trigger = Trigger.create () in
  if Computation.try_attach computation trigger then
    match Trigger.await trigger with
    | None -> ()
    | Some (exn, bt) ->
        if Computation.try_cancel computation Exit bt then
          Printexc.raise_with_backtrace exn bt

(* *)

let rec give t value backoff =
  let before = Atomic.fenceless_get t in
  match before.takers with
  | Q.T Zero ->
      let computation = Computation.create ~mode:`LIFO () in
      let self = G { computation; result = Fun.id; value } in
      let givers = Q.add before.givers self in
      let after = { givers; takers = T Zero } in
      if Atomic.compare_and_set t before after then wait computation
      else give t value (Backoff.once backoff)
  | Q.T (One _ as takers) ->
      let (T { computation; result }) = Q.head takers in
      let got = Computation.try_return computation (fun () -> result value) in
      let takers = Q.tail takers in
      let givers = before.givers in
      let after =
        if takers == T Zero && givers == T Zero then empty
        else { givers; takers }
      in
      let no_contention = Atomic.compare_and_set t before after in
      if not got then
        give t value (if no_contention then backoff else Backoff.once backoff)

let rec take t backoff =
  let before = Atomic.fenceless_get t in
  match before.givers with
  | Q.T Zero ->
      let computation = Computation.create ~mode:`LIFO () in
      let self = T { computation; result = Fun.id } in
      let takers = Q.add before.takers self in
      let after = { givers = T Zero; takers } in
      if Atomic.compare_and_set t before after then begin
        wait computation;
        Computation.await computation ()
      end
      else take t (Backoff.once backoff)
  | Q.T (One _ as givers) ->
      let (G { computation; result; value }) = Q.head givers in
      let got = Computation.try_return computation result in
      let givers = Q.tail givers in
      let takers = before.takers in
      let after =
        if givers == T Zero && takers == T Zero then empty
        else { givers; takers }
      in
      let no_contention = Atomic.compare_and_set t before after in
      if got then value
      else take t (if no_contention then backoff else Backoff.once backoff)

(* *)

let rec give_as t (G gr as self) before selfs (Cons head_r as head : _ S.cons)
    tail =
  let (T tr as taker) = head_r.value in
  if Tx.same tr.computation gr.computation then
    let selfs = S.cons taker selfs in
    give_as_advance t self before selfs head tail
  else
    let tx = Tx.create () in
    let result = tr.result in
    let value = gr.value in
    if not (Tx.try_return tx tr.computation (fun () -> result value)) then
      give_as_advance t self before selfs head tail
    else if
      (not (Tx.try_return tx gr.computation gr.result))
      || not (Tx.try_commit tx)
    then
      if not (Computation.is_running gr.computation) then ( (* TODO *) )
      else if Computation.is_running tr.computation then
        give_as t self before selfs head tail
      else give_as_advance t self before selfs head tail
    else
      let takers =
        if head == tail then Q.reverse_as_queue selfs
        else
          let head = S.reverse_to (S.as_cons head_r.next) selfs in
          Q.T (One { head; tail; cons = tail })
      in
      let givers = before.givers in
      let after =
        if takers == Q.T Zero && givers == Q.T Zero then empty
        else { givers; takers }
      in
      if not (Atomic.compare_and_set t before after) then
        ( (* TODO: avoid leak *) )

and give_as_advance t self before selfs (Cons head_r as head : _ S.cons) tail =
  if head != tail then give_as t self before selfs (S.as_cons head_r.next) tail
  else
    let takers = Q.reverse_as_queue selfs in
    let givers = Q.add before.givers self in
    let after = { givers; takers } in
    if not (Atomic.compare_and_set t before after) then give_as_start t self

and give_as_start t self =
  let before = Atomic.get t in
  match before.takers with
  | Q.T Zero ->
      let takers = Q.T Zero in
      let givers = Q.singleton self in
      let after = { givers; takers } in
      if not (Atomic.compare_and_set t before after) then give_as_start t self
  | Q.T (One r as o) ->
      Q.exec o;
      give_as t self before (T Nil) r.head r.cons

let give_evt t value =
  let request computation result =
    give_as_start t (G { computation; result; value })
  in
  Event.from_request { request }

(* *)

let rec take_as t (T tr as self) before selfs (Cons head_r as head : _ S.cons)
    tail =
  let (G gr as giver) = head_r.value in
  if Tx.same tr.computation gr.computation then
    let selfs = S.cons giver selfs in
    take_as_advance t self before selfs head tail
  else
    let tx = Tx.create () in
    let result = tr.result in
    let value = gr.value in
    if not (Tx.try_return tx gr.computation gr.result) then
      take_as_advance t self before selfs head tail
    else if
      (not (Tx.try_return tx tr.computation (fun () -> result value)))
      || not (Tx.try_commit tx)
    then
      if not (Computation.is_running gr.computation) then ( (* TODO *) )
      else if Computation.is_running tr.computation then
        take_as t self before selfs head tail
      else take_as_advance t self before selfs head tail
    else
      let takers = before.takers in
      let givers =
        if head == tail then Q.reverse_as_queue selfs
        else
          let head = S.reverse_to (S.as_cons head_r.next) selfs in
          Q.T (One { head; tail; cons = tail })
      in
      let after =
        if takers == Q.T Zero && givers == Q.T Zero then empty
        else { givers; takers }
      in
      if not (Atomic.compare_and_set t before after) then
        ( (* TODO: avoid leak *) )

and take_as_advance t self before selfs (Cons head_r as head : _ S.cons) tail =
  if head != tail then take_as t self before selfs (S.as_cons head_r.next) tail
  else
    let givers = Q.reverse_as_queue selfs in
    let takers = Q.add before.takers self in
    let after = { givers; takers } in
    if not (Atomic.compare_and_set t before after) then take_as_start t self

and take_as_start t self =
  let before = Atomic.get t in
  match before.givers with
  | Q.T Zero ->
      let givers = Q.T Zero in
      let takers = Q.singleton self in
      let after = { givers; takers } in
      if not (Atomic.compare_and_set t before after) then take_as_start t self
  | Q.T (One r as o) ->
      Q.exec o;
      take_as t self before (T Nil) r.head r.cons

let take_evt t =
  let request computation result =
    take_as_start t (T { computation; result })
  in
  Event.from_request { request }

(* *)

let[@inline] take t = take t Backoff.default
let[@inline] give t value = give t value Backoff.default
