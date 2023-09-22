open Picos

type 'a t = ('a, [ `Await | `Cancel ]) Computation.t
type 'a unpublished = ('a, [ `Await | `Cancel | `Return ]) Computation.t

let await = Computation.await
let[@inline] of_computation x = (x :> 'a t)
let peek t = if Computation.is_running t then None else Some (await t)
let cancel = Computation.cancel

module Infix = struct
  let ( let+ ) x xy =
    let y = Computation.create () in
    let main = Computation.capture y @@ fun () -> xy (await x) in
    Fiber.spawn ~forbid:false y [ main ];
    of_computation y

  let ( and+ ) x y =
    let xy = Computation.create () in
    let main =
      Computation.capture xy @@ fun () ->
      let c = Computation.canceler ~from:y ~into:xy in
      if Computation.try_attach y c then
        let x =
          match await x with
          | x ->
              Computation.detach y c;
              x
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              Computation.detach y c;
              Printexc.raise_with_backtrace exn bt
        in
        (x, await y)
      else
        let y = await y in
        (await x, y)
    in
    Fiber.spawn ~forbid:false xy [ main ];
    of_computation xy

  let ( let* ) x xy =
    let y = Computation.create () in
    let main = Computation.capture y @@ fun () -> await (xy (await x)) in
    Fiber.spawn ~forbid:false y [ main ];
    of_computation y

  let ( and* ) = ( and+ )
end

let both x y =
  let open Infix in
  let+ () = x and+ () = y in
  ()

let create = Computation.create
let return_to = Computation.return
let reify_to t thunk = Computation.capture t thunk ()
let publish = of_computation
