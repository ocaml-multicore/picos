open Picos
module S = Picos_std_sync

module List_ext = struct
  let[@tail_mod_cons] rec drop_first_or_not_found x' = function
    | [] -> raise_notrace Not_found
    | x :: xs -> if x == x' then xs else x :: drop_first_or_not_found x' xs
end

module Id = struct
  type t = T of int [@@unboxed]

  let[@inline] compare (T l) (T r) = Int.compare l r
  let key = Fiber.FLS.create ()
  let next_id = Atomic.make 0

  let get_as fiber =
    match Fiber.FLS.get_exn fiber key with
    | id -> id
    | exception Fiber.FLS.Not_set ->
        let id = T (Atomic.fetch_and_add next_id 1) in
        Fiber.FLS.set fiber key id;
        id

  let get () = get_as @@ Fiber.current ()
end

module Priority = struct
  type t = T of int [@@unboxed]

  let[@inline] compare (T l) (T r) = Int.compare l r
  let[@inline] max (T l) (T r) = T (Int.max l r)
  let default = T 0
  let higher (T p) = T (p + 1)
  let key = Fiber.FLS.create ()
  let get_as fiber = Fiber.FLS.get fiber key ~default
  let get () = get_as @@ Fiber.current ()

  let set priority =
    let fiber = Fiber.current () in
    if priority = default then Fiber.FLS.remove fiber key
    else Fiber.FLS.set fiber key priority
end

module Priority_inv = struct
  type t = Priority.t

  let compare l r = Priority.compare r l
end

module Pq_hi = Psq.Make (Id) (Priority_inv)

(*
type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Holder :
*)
type mutex = { waiters : Pq_hi.t Atomic.t; mutex : S.Mutex.t }

module Mutex = struct
  type t = mutex

  let key = Fiber.FLS.create ()
  let get_as fiber = Fiber.FLS.get fiber key ~default:[]

  let add_mutex_as fiber t =
    get_as fiber |> List.cons t |> Fiber.FLS.set fiber key

  let remove_mutex_as fiber t =
    get_as fiber
    |> List_ext.drop_first_or_not_found t
    |> Fiber.FLS.set fiber key

  let rec add_waiter t id priority backoff =
    let before = Atomic.get t.waiters in
    let after = Pq_hi.add id priority before in
    if not (Atomic.compare_and_set t.waiters before after) then
      add_waiter t id priority (Backoff.once backoff)

  let rec remove_waiter t id backoff =
    let before = Atomic.get t.waiters in
    let after = Pq_hi.remove id before in
    if not (Atomic.compare_and_set t.waiters before after) then
      remove_waiter t id (Backoff.once backoff)

  let max_waiter t =
    match Pq_hi.min (Atomic.get t.waiters) with
    | None -> Priority.default
    | Some (_id, priority) -> priority

  let create ?padded () =
    let waiters = Atomic.make Pq_hi.empty |> Multicore_magic.copy_as ?padded in
    let mutex = S.Mutex.create ?padded () in
    Multicore_magic.copy_as ?padded { waiters; mutex }

  let lock t =
    let fiber = Fiber.current () in
    let id = Id.get_as fiber in
    let priority = Priority.get_as fiber in
    add_waiter t id priority Backoff.default;
    match S.Mutex.lock t.mutex with
    | () ->
        remove_waiter t id Backoff.default;
        add_mutex_as fiber t
    | exception exn ->
        remove_waiter t id Backoff.default;
        raise exn

  let unlock t =
    let fiber = Fiber.current () in
    remove_mutex_as fiber t;
    S.Mutex.unlock t.mutex
end

module Condition = struct
  type t = S.Condition.t

  let create = S.Condition.create
  let wait t m = S.Condition.wait t m.mutex
  let broadcast = S.Condition.broadcast
end

let _get_dynamic_priority_as fiber =
  Mutex.get_as fiber
  |> List.fold_left
       (fun p m -> Priority.max p (Mutex.max_waiter m))
       (Priority.get_as fiber)

let run_fiber ?fatal_exn_handler:_ _fiber _main = failwith "XXX"
let run ?fatal_exn_handler:_ ?forbid:_ _main = failwith "XXX"
