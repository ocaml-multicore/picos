type 'c t = {
  current : 'c -> Fiber.t;
  spawn : 'c -> Fiber.t -> (Fiber.t -> unit) -> unit;
  yield : 'c -> unit;
  cancel_after :
    'a.
    'c ->
    'a Computation.t ->
    seconds:float ->
    exn ->
    Printexc.raw_backtrace ->
    unit;
  await : 'c -> Trigger.t -> (exn * Printexc.raw_backtrace) option;
}
