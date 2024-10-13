let read_lines path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let lines = ref [] in
  while
    match input_line ch with
    | line ->
        lines := line :: !lines;
        true
    | exception End_of_file -> false
  do
    ()
  done;
  List.rev !lines

let file path =
  Printf.printf "# 1 \"lib/picos/%s\"\n" path;
  read_lines path |> List.iter print_endline

let line text = print_endline text

let () =
  let picos_ocaml5_ml = read_lines "picos.ocaml5.ml" in
  let copy label =
    let begin_marker = Printf.sprintf "(* BEGIN %s *)" label in
    let end_marker = Printf.sprintf "(* END %s *)" label in
    let ends_with ~suffix s =
      let n = String.length suffix in
      n <= String.length s && String.sub s (String.length s - n) n = suffix
    in
    let rec find_begin i = function
      | [] -> failwith "begin marker not found"
      | line :: lines ->
          let i = i + 1 in
          if ends_with ~suffix:begin_marker line then begin
            Printf.printf "# %d \"lib/picos/picos.ocaml5.ml\"\n" i;
            copy_to_end lines
          end
          else find_begin i lines
    and copy_to_end = function
      | [] -> failwith "end marker not found"
      | line :: lines ->
          if not (ends_with ~suffix:end_marker line) then begin
            print_endline line;
            copy_to_end lines
          end
    in
    find_begin 1 picos_ocaml5_ml
  in

  line "module Bootstrap = struct";

  line "module Trigger = struct";
  copy "TRIGGER BOOTSTRAP";
  line "end";

  line "module Computation = struct";
  copy "COMPUTATION BOOTSTRAP";
  line "end";

  line "module Fiber = struct";
  copy "FIBER BOOTSTRAP";
  line "end";

  line "end";

  line "module Handler = struct";
  line "open Bootstrap";
  copy "HANDLER BOOTSTRAP";
  file "handler.ocaml4.ml";
  line "end";

  line "module Trigger = struct";
  line "include Bootstrap.Trigger";
  file "trigger.ocaml4.ml";
  line "end";

  line "module Computation = struct";
  line "include Bootstrap.Computation";
  file "computation.ocaml4.ml";
  copy "COMPUTATION COMMON";
  line "end";

  line "module Fiber = struct";
  line "include Bootstrap.Fiber";
  file "fiber.ocaml4.ml";
  copy "FIBER COMMON";
  line "end"
