open Picos

type per_fiber = { mutable lo : int; mutable hi : int }

type _ tdt =
  | Empty : [> `Empty ] tdt
  | Range : {
      mutable _lo : int;
      hi : int;
      parent : [ `Empty | `Range ] tdt;
    }
      -> [> `Range ] tdt

external lo_as_atomic : [ `Range ] tdt -> int Atomic.t = "%identity"

let rec for_out t (Range r as range : [ `Range ] tdt) per_fiber action =
  let lo_before = Atomic.get (lo_as_atomic range) in
  let n = r.hi - lo_before in
  if 0 < n then begin
    let lo_after = lo_before + 1 + (n asr 1) in
    if Atomic.compare_and_set (lo_as_atomic range) lo_before lo_after then begin
      per_fiber.lo <- lo_before;
      per_fiber.hi <- lo_after;
      while Bundle.is_running t && per_fiber.lo < per_fiber.hi do
        try
          while per_fiber.lo < per_fiber.hi do
            let i = per_fiber.lo in
            per_fiber.lo <- i + 1;
            action i
          done
        with exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
      done
    end;
    for_out t range per_fiber action
  end
  else
    match r.parent with
    | Empty -> ()
    | Range _ as range -> for_out t range per_fiber action

let rec for_in t (Range r as range : [ `Range ] tdt) per_fiber action =
  let lo_before = Atomic.get (lo_as_atomic range) in
  let n = r.hi - lo_before in
  if n <= 1 then for_out t range per_fiber action
  else
    let lo_after = lo_before + (n asr 1) in
    if Atomic.compare_and_set (lo_as_atomic range) lo_before lo_after then begin
      Bundle.fork t (fun () -> for_in_enter t range action);
      let child = Range { _lo = lo_before; hi = lo_after; parent = range } in
      for_in t child per_fiber action
    end
    else for_in t range per_fiber action

and for_in_enter bundle range action =
  let per_fiber = { lo = 0; hi = 0 } in
  let effc (type a) :
      a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
    | Fiber.Spawn _ | Fiber.Current | Computation.Cancel_after _ -> None
    | _ ->
        (* Might be blocking, so fork any remaining work to another fiber. *)
        if per_fiber.lo < per_fiber.hi then begin
          let range =
            Range { _lo = per_fiber.lo; hi = per_fiber.hi; parent = Empty }
          in
          per_fiber.lo <- per_fiber.hi;
          Bundle.fork bundle (fun () -> for_in_enter bundle range action)
        end;
        None
  in
  let handler = Effect.Deep.{ effc } in
  Effect.Deep.try_with (for_in bundle range per_fiber) action handler

let for_n ?on_terminate n action =
  if 0 < n then
    if n = 1 then
      try action 0
      with
      | Control.Terminate when Bundle.on_terminate on_terminate == `Ignore ->
        ()
    else
      let range = Range { _lo = 0; hi = n; parent = Empty } in
      Bundle.join_after ?on_terminate @@ fun t -> for_in_enter t range action
