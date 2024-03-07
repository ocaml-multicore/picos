type t = { mutex : Mutex.t; condition : Condition.t }

let mutex_and_condition_key =
  Thread_local_storage.new_key @@ fun () ->
  { mutex = Mutex.create (); condition = Condition.create () }

let get () = Thread_local_storage.get mutex_and_condition_key
let lock t = Mutex.lock t.mutex
let unlock t = Mutex.unlock t.mutex

let broadcast t =
  lock t;
  unlock t;
  Condition.broadcast t.condition

let wait t = Condition.wait t.condition t.mutex
