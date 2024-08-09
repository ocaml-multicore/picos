open Picos_bootstrap

let[@inline never] error _ =
  raise (Sys_error "Picos.Handler.using not called for current thread")

module Handler = struct
  type entry = E : { context : 'a; handler : 'a Handler.t } -> entry

  let default =
    let current = error
    and spawn _ _ = error
    and yield = error
    and cancel_after _ _ ~seconds:_ = error
    and await _ = error in
    E { context = (); handler = { current; spawn; yield; cancel_after; await } }

  let key = Picos_thread.TLS.new_key @@ fun () -> default
  let get () = Picos_thread.TLS.get key

  let using handler context main =
    let old = Picos_thread.TLS.get key in
    Picos_thread.TLS.set key (E { context; handler });
    match main (handler.current context) with
    | value ->
        Picos_thread.TLS.set key old;
        value
    | exception exn ->
        Picos_thread.TLS.set key old;
        raise exn
end

module Trigger = struct
  let await t =
    if Trigger.is_initial t then
      let (E r) = Handler.get () in
      r.handler.await r.context t
    else None
end

module Fiber = struct
  let current () =
    let (E r) = Handler.get () in
    r.handler.current r.context

  let spawn fiber main =
    let (E r) = Handler.get () in
    r.handler.spawn r.context fiber main

  let yield () =
    let (E r) = Handler.get () in
    r.handler.yield r.context
end

module Computation = struct
  let cancel_after computation ~seconds exn_bt =
    Computation.check_non_negative seconds;
    let (E r) = Handler.get () in
    r.handler.cancel_after r.context computation ~seconds exn_bt
end
