let main_thread = Thread.id (Thread.self ())
let is_main_thread () = Thread.id (Thread.self ()) = main_thread

module TLS = Thread_local_storage
