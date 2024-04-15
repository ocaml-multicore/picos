open Picos

let () = Picos_select.configure ()

let test_intr () =
  let inn, out = Unix.pipe ~cloexec:true () in
  let main () =
    Picos_threaded.run ~forbid:false @@ fun () ->
    let computation = Computation.create () in
    let n_threads = 10 in
    let n = Atomic.make n_threads in
    for _ = 1 to n_threads do
      let main () =
        try
          for _ = 1 to 1_000 do
            let req = Picos_select.Intr.req ~seconds:0.000_001 in
            match Unix.read inn (Bytes.create 1) 0 1 with
            | _ -> assert false
            | exception Unix.Unix_error (EINTR, _, _) ->
                Picos_select.Intr.clr req
          done;
          if 1 = Atomic.fetch_and_add n (-1) then Computation.finish computation
        with exn -> Computation.cancel computation (Exn_bt.get exn)
      in
      Fiber.spawn ~forbid:false computation [ main ]
    done;
    Computation.await computation
  in
  let domains = Array.init 3 @@ fun _ -> Domain.spawn main in
  let finally () =
    Array.iter Domain.join domains;
    Unix.close inn;
    Unix.close out
  in
  Fun.protect ~finally main

let () =
  [
    ( "Intr",
      if Sys.win32 || String.starts_with ~prefix:"5.0." Sys.ocaml_version then
        []
      else [ Alcotest.test_case "" `Quick test_intr ] );
  ]
  |> Alcotest.run "Picos_select"
