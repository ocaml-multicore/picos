open Picos

let[@inline never] impossible () = failwith "impossible"
let[@inline never] not_in_a_hoot () = invalid_arg "Not in a hoot"

module Message = struct
  type t = ..
end

type _ tdt =
  | Nil : [> `Nil ] tdt
  | Message : {
      message : Message.t;
      next : [ `Nil | `Message ] tdt;
    }
      -> [> `Message ] tdt
  | Wait : Trigger.t -> [> `Wait ] tdt
  | Pid : {
      computation : unit Computation.t;
      terminated : unit Computation.t;
      incoming : incoming Atomic.t;
      mutable received : [ `Nil | `Message ] tdt;
    }
      -> [> `Pid ] tdt

and incoming = In : [< `Nil | `Message | `Wait ] tdt -> incoming [@@unboxed]

module Pid = struct
  type t = [ `Pid ] tdt

  let key : [ `Nil | `Pid ] tdt Fiber.FLS.t = Fiber.FLS.create ()
end

let self_of fiber =
  match Fiber.FLS.get_exn fiber Pid.key with
  | Pid _ as t -> t
  | Nil | (exception Fiber.FLS.Not_set) -> not_in_a_hoot ()

let self () : Pid.t = self_of @@ Fiber.current ()

exception Terminate

let run main =
  let fiber = Fiber.current () in
  let before = Fiber.FLS.get fiber Pid.key ~default:Nil in
  let computation = Computation.create ~mode:`LIFO () in
  let inner = Computation.Packed computation in
  let (Pid p as t) : Pid.t =
    let terminated = Computation.create ~mode:`LIFO ()
    and incoming = Atomic.make (In Nil) |> Multicore_magic.copy_as_padded in
    Pid { computation; terminated; incoming; received = Nil }
  in
  Fiber.FLS.set fiber Pid.key t;
  let (Packed parent as outer) = Fiber.get_computation fiber in
  let canceler = Computation.attach_canceler ~from:parent ~into:computation in
  Fiber.set_computation fiber inner;
  begin
    match main () with
    | () | (exception Terminate) -> Computation.finish p.terminated
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Computation.cancel p.terminated exn bt
  end;
  Computation.finish p.computation;
  Fiber.set_computation fiber outer;
  Computation.detach parent canceler;
  Fiber.FLS.set fiber Pid.key before;
  (* An otherwise unhandled exception except [Terminate] will be raised. *)
  Computation.check p.terminated

let wait (Pid p : Pid.t) = Computation.wait p.terminated

let spawn main =
  let (Pid p as t) : Pid.t =
    let computation = Computation.create ~mode:`LIFO ()
    and terminated = Computation.create ~mode:`LIFO ()
    and incoming = Atomic.make (In Nil) in
    Pid { computation; terminated; incoming; received = Nil }
  in
  let fiber = Fiber.create ~forbid:false p.computation in
  Fiber.FLS.set fiber Pid.key t;
  begin
    Fiber.spawn fiber @@ fun fiber ->
    let (Pid p) : Pid.t = self_of fiber in
    (* An unhandled exception except [Terminate] will be treated as a fatal
       error. *)
    begin
      match main () with
      | () | (exception Terminate) -> Computation.finish p.terminated
    end;
    Computation.finish p.computation
  end;
  t

let rec rev_to (Message _ as ms : [ `Message ] tdt) :
    [ `Nil | `Message ] tdt -> _ = function
  | Nil -> ms
  | Message r -> rev_to (Message { message = r.message; next = ms }) r.next

let rev (Message r : [ `Message ] tdt) =
  rev_to (Message { message = r.message; next = Nil }) r.next

let rec receive (Pid p as t : Pid.t) =
  match Atomic.get p.incoming with
  | In Nil as before ->
      let trigger = Trigger.create () in
      let after = In (Wait trigger) in
      if Atomic.compare_and_set p.incoming before after then begin
        match Trigger.await trigger with
        | None -> ()
        | Some (exn, bt) ->
            (* At this point the trigger has been signaled and cannot leak
               arbitrary amount of space.  There is no need to remove it. *)
            Printexc.raise_with_backtrace exn bt
      end;
      receive t
  | _ -> begin
      match Atomic.exchange p.incoming (In Nil) with
      | In (Wait _ | Nil) -> impossible ()
      | In (Message _ as ms) ->
          let (Message r : [ `Message ] tdt) = rev ms in
          p.received <- r.next;
          r.message
    end

let receive () =
  let (Pid p as t) = self () in
  match p.received with
  | Message r ->
      p.received <- r.next;
      r.message
  | Nil -> receive t

let rec send (Pid p as t : Pid.t) message backoff =
  match Atomic.get p.incoming with
  | In ((Nil | Message _) as before) ->
      let after = Message { message; next = before } in
      if not (Atomic.compare_and_set p.incoming (In before) (In after)) then
        send t message (Backoff.once backoff)
  | In (Wait trigger as before) ->
      let after = Message { message; next = Nil } in
      if Atomic.compare_and_set p.incoming (In before) (In after) then
        Trigger.signal trigger
      else send t message (Backoff.once backoff)

let[@inline] send t message = send t message Backoff.default

type Message.t += Terminated of Pid.t

let monitor ~at ~the:(Pid the_p as the : Pid.t) =
  let[@alert "-handler"] trigger =
    Trigger.from_action at the @@ fun _ at the -> send at (Terminated the)
  in
  if not (Computation.try_attach the_p.terminated trigger) then
    send at (Terminated the)

let empty_bt = Printexc.get_callstack 0

let link (Pid p1 as t1 : Pid.t) (Pid p2 as t2 : Pid.t) =
  let[@alert "-handler"] trigger =
    Trigger.from_action t1 t2 @@ fun _ (Pid p1 : Pid.t) (Pid p2 : Pid.t) ->
    Computation.cancel p1.computation Terminate empty_bt;
    Computation.cancel p2.computation Terminate empty_bt
  in
  if
    (not (Computation.try_attach p1.terminated trigger))
    || not (Computation.try_attach p2.terminated trigger)
  then begin
    Computation.cancel p1.computation Terminate empty_bt;
    Computation.cancel p2.computation Terminate empty_bt
  end
