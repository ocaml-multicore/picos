//Provides: caml_thread_initialize
function caml_thread_initialize(unit) {
  return unit
}

// Provides: caml_thread
var caml_thread = [0, 0, 0, 0]

//Provides: caml_thread_self
//Requires: caml_ml_domain_id, caml_failwith, caml_thread
function caml_thread_self(unit) {
  if (caml_ml_domain_id(unit) != caml_thread[1])
    caml_failwith('caml_thread_self: spawning of domains is not supported')
  return caml_thread
}

//Provides: caml_thread_id
function caml_thread_id(thread) {
  return thread[1]
}
