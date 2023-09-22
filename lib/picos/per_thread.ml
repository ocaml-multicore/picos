type t = {
  mutex : Mutex.t;
  condition : Condition.t;
  fiber : [ `Sync | `Async ] Bootstrap.Fiber.t;
}

open struct
  let create fiber =
    let mutex = Mutex.create () and condition = Condition.create () in
    { mutex; condition; fiber }

  let per_thread_key =
    TLS.new_key @@ fun () ->
    create
      (Bootstrap.Fiber.create ~forbid:false (Bootstrap.Computation.create ()))
end

let get () = TLS.get per_thread_key
let set fiber = TLS.set per_thread_key (create fiber)
