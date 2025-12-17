open Picos

val run_fiber :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  Fiber.t ->
  (Fiber.t -> unit) ->
  unit

val run :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  ?forbid:bool ->
  (unit -> 'a) ->
  'a
