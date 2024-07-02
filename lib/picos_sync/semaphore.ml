open Picos

let[@inline never] overflow () = raise (Sys_error "overflow")
let[@inline never] negative () = invalid_arg "negative initial count"

(* TODO: This is not the fastest nor the most scalable implementation. *)

type semaphore = { mutex : Mutex.t; nonzero : Condition.t; mutable count : int }

module Counting = struct
  type t = semaphore

  let make ?padded count =
    if count < 0 then negative ();
    let mutex = Mutex.create ?padded () in
    let nonzero = Condition.create ?padded () in
    let t = { mutex; nonzero; count } in
    match padded with
    | None | Some false -> t
    | Some true -> Multicore_magic.copy_as_padded t

  let release t =
    Mutex.lock ~checked:false t.mutex;
    let count = t.count in
    if count < count + 1 then t.count <- count + 1;
    Mutex.unlock ~checked:false t.mutex;
    if count = 0 then Condition.signal t.nonzero
    else if count + 1 < count then overflow ()

  let acquire t =
    Mutex.lock ~checked:false t.mutex;
    match
      while t.count = 0 do
        Condition.wait t.nonzero t.mutex
      done
    with
    | () ->
        t.count <- t.count - 1;
        Mutex.unlock ~checked:false t.mutex
    | exception exn ->
        let exn_bt = Exn_bt.get exn in
        Mutex.unlock ~checked:false t.mutex;
        Exn_bt.raise exn_bt

  let try_acquire t =
    Mutex.lock ~checked:false t.mutex;
    let count = t.count in
    if 0 < count then t.count <- count - 1;
    Mutex.unlock ~checked:false t.mutex;
    0 < count

  let get_value t =
    if Mutex.try_lock ~checked:false t.mutex then
      Mutex.unlock ~checked:false t.mutex;
    t.count
end

module Binary = struct
  type t = semaphore

  let make ?padded initial = Counting.make ?padded (Bool.to_int initial)

  let release t =
    Mutex.lock ~checked:false t.mutex;
    t.count <- 1;
    Mutex.unlock ~checked:false t.mutex;
    Condition.signal t.nonzero

  let acquire = Counting.acquire
  let try_acquire = Counting.try_acquire
end
