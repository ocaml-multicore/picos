open Meow

let test_ownership_finalized_on_error () =
  let resource = ref 1 in
  Test_scheduler.run @@ fun () ->
  let promise =
    Promise.async ~give:[ Ownership.create ~finally:decr resource ] @@ fun () ->
    raise Not_found
  in
  match Promise.await promise with
  | () -> assert false
  | exception Not_found -> assert (!resource = 0)

let () =
  [
    ( "Ownership",
      [
        Alcotest.test_case "finalized on error" `Quick
          test_ownership_finalized_on_error;
      ] );
  ]
  |> Alcotest.run "Meow"
