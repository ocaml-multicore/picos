open Picos_structured
open Picos_finally

let () = Picos_select.configure ()

let test_intr () =
  let@ inn, _out =
    finally (fun (inn, out) ->
        Unix.close inn;
        Unix.close out)
    @@ Unix.pipe ~cloexec:true
  in
  let main () =
    Picos_threaded.run @@ fun () ->
    Bundle.join_after @@ fun bundle ->
    for _ = 1 to 10 do
      Bundle.fork bundle @@ fun () ->
      for _ = 1 to 1_000 do
        let req = Picos_select.Intr.req ~seconds:0.000_001 in
        match Unix.read inn (Bytes.create 1) 0 1 with
        | _ -> assert false
        | exception Unix.Unix_error (EINTR, _, _) -> Picos_select.Intr.clr req
      done
    done
  in
  let@ _domains =
    finally (Array.iter Domain.join) @@ fun () ->
    Array.init 3 @@ fun _ -> Domain.spawn main
  in
  main ()

let () =
  [
    ( "Intr",
      if Sys.win32 || String.starts_with ~prefix:"5.0." Sys.ocaml_version then
        []
      else [ Alcotest.test_case "" `Quick test_intr ] );
  ]
  |> Alcotest.run "Picos_select"
