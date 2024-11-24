//Provides: caml_thread_initialize
function caml_thread_initialize(unit) {
  return unit
}

// Provides: caml_thread
var caml_thread = [0, 0, 0, 0]

//Provides: caml_thread_self
//Requires: caml_thread
function caml_thread_self(unit) {
  // There are no threads other than the main thread.
  return caml_thread
}

//Provides: caml_thread_id
function caml_thread_id(thread) {
  return thread[1]
}

//Provides: caml_mutex_new
function caml_mutex_new(unit) {
  return unit
}
