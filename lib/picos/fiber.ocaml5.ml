let resume t k = Effect.Deep.continue k (canceled t)
let resume_with t k h = Effect.Shallow.continue_with k (canceled t) h

let continue t k v =
  match canceled t with
  | None -> Effect.Deep.continue k v
  | Some (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt

let continue_with t k v h =
  match canceled t with
  | None -> Effect.Shallow.continue_with k v h
  | Some (exn, bt) -> Effect.Shallow.discontinue_with_backtrace k exn bt h

type _ Effect.t += Current : t Effect.t

let current () = Effect.perform Current

type _ Effect.t += Spawn : { fiber : t; main : t -> unit } -> unit Effect.t

let spawn fiber main = Effect.perform @@ Spawn { fiber; main }

type _ Effect.t += Yield : unit Effect.t

let yield () = Effect.perform Yield
