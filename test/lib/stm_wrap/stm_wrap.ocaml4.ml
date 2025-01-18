open Picos

let[@inline never] error entry _ = failwith entry

let handler =
  let current fiber = fiber
  and spawn _ _ = error "spawn"
  and yield = error "yield"
  and cancel_after _ _ ~seconds:_ _ = error "cancel_after"
  and await _ t =
    while not (Trigger.is_signaled t) do
      Thread.yield ()
    done;
    None
  in
  Handler.{ current; spawn; yield; cancel_after; await }

let wrap_cmd_seq th =
  let result = ref (Error Exit) in
  Handler.using handler
    (Fiber.create ~forbid:false (Computation.create ()))
    (fun _fiber ->
      result :=
        match th () with value -> Ok value | exception exn -> Error exn);
  match !result with Ok value -> value | Error exn -> raise exn
