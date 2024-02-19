open Picos
open Foundation

type 'a state =
  | Fun of (unit -> 'a)
  | Run of { fiber : Fiber.as_async; triggers : Trigger.as_signal list }
  | Val of 'a
  | Exn of { exn : exn; trace : Printexc.raw_backtrace }

type 'a t = 'a state Atomic.t

let from_fun th = Atomic.make (Fun th)
let from_val v = Atomic.make (Val v)

let rec cleanup t trigger backoff =
  match Atomic.get t with
  | Val _ | Exn _ -> ()
  | Fun _ -> failwith "impossible"
  | Run r as before -> begin
      match List.drop_first_or_not_found trigger r.triggers with
      | triggers ->
          let after = Run { r with triggers } in
          if not (Atomic.compare_and_set t before after) then
            cleanup t trigger (Backoff.once backoff)
      | exception Not_found -> ()
    end

let rec force t fiber backoff =
  match Atomic.get t with
  | Val v -> v
  | Exn r -> Printexc.raise_with_backtrace r.exn r.trace
  | Fun th as before ->
      let after = Run { fiber; triggers = [] } in
      if Atomic.compare_and_set t before after then begin
        let result =
          match th () with
          | v -> Val v
          | exception exn ->
              let trace = Printexc.get_raw_backtrace () in
              Exn { exn; trace }
        in
        match Atomic.exchange t result with
        | Val _ | Exn _ | Fun _ -> failwith "impossible"
        | Run r ->
            List.iter Trigger.signal r.triggers;
            force t fiber Backoff.default
      end
      else force t fiber (Backoff.once backoff)
  | Run r as before ->
      if Fiber.equal r.fiber fiber then raise Stdlib.Lazy.Undefined
      else
        let trigger = Trigger.create () in
        let triggers = (trigger :> Trigger.as_signal) :: r.triggers in
        let after = Run { r with triggers } in
        if Atomic.compare_and_set t before after then begin
          match Trigger.await trigger with
          | None -> force t fiber Backoff.default
          | Some exn_bt ->
              cleanup t (trigger :> Trigger.as_signal) Backoff.default;
              Exn_bt.raise exn_bt
        end
        else force t fiber (Backoff.once backoff)

let force t =
  match Atomic.get t with
  | Val v -> v
  | Exn r -> Printexc.raise_with_backtrace r.exn r.trace
  | Fun _ | Run _ ->
      force t (Fiber.current () :> Fiber.as_async) Backoff.default

let map f t = from_fun @@ fun () -> f (force t)
