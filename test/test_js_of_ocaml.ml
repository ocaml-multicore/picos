open Picos

let () =
  Printf.printf "Hello, from js_of_ocaml with Picos!\n%!";
  let trigger = Trigger.create () in
  Trigger.signal trigger
