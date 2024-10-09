let[@inline never] error _ =
  raise (Sys_error "Picos.Handler.using not called for current thread")

type entry = E : { context : 'a; handler : 'a t } -> entry

let default =
  let current = error
  and spawn _ _ = error
  and yield = error
  and cancel_after _ _ ~seconds:_ _ = error
  and await _ = error in
  E { context = (); handler = { current; spawn; yield; cancel_after; await } }

let key = Picos_thread.TLS.create ()
let get () = Picos_thread.TLS.get_exn key

let using handler context main =
  let old =
    try Picos_thread.TLS.get_exn key with Picos_thread.TLS.Not_set -> default
  in
  Picos_thread.TLS.set key (E { context; handler });
  match main (handler.current context) with
  | value ->
      Picos_thread.TLS.set key old;
      value
  | exception exn ->
      Picos_thread.TLS.set key old;
      raise exn
