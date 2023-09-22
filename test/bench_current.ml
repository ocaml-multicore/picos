open Picos

let () =
  Fifos.run ~forbid:false @@ fun () ->
  let n = 10_000_000 in

  let start = Unix.gettimeofday () in

  for _ = 1 to n do
    Fiber.current () |> ignore
  done;

  let elapsed = Unix.gettimeofday () -. start in

  Printf.printf "Fiber.current (): %f per second\n%!" (Float.of_int n /. elapsed)
