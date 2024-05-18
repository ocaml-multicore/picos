open Picos_structured.Finally
open Multicore_bench

let lib_suffix = if Sys.os_type == "Win32" then "lib" else "a"

let paths =
  let lib name = (name, "lib/" ^ name ^ "/" ^ name ^ "." ^ lib_suffix) in
  [
    lib "picos";
    lib "picos_domain";
    lib "picos_exn_bt";
    lib "picos_fd";
    lib "picos_fifos";
    lib "picos_htbl";
    lib "picos_lwt";
    lib "picos_mpscq";
    lib "picos_randos";
    lib "picos_rc";
    lib "picos_select";
    lib "picos_stdio";
    lib "picos_structured";
    lib "picos_sync";
    lib "picos_thread";
    lib "picos_threaded";
  ]

let run_suite ~budgetf:_ =
  match
    [ "_build/default"; "../../../_build/default" ]
    |> List.find @@ fun prefix ->
       match Unix.opendir prefix with
       | dir ->
           Unix.closedir dir;
           true
       | exception Unix.Unix_error (ENOENT, _, _) -> false
  with
  | prefix -> begin
      paths
      |> List.concat_map @@ fun (name, path) ->
         match
           let@ file =
             finally Unix.close @@ fun () ->
             Unix.openfile (prefix ^ "/" ^ path) [] 0
           in
           Unix.fstat file
         with
         | stats ->
             [
               Metric.make ~metric:"binary size" ~config:name ~units:"kB"
                 ~trend:`Lower_is_better ~description:"Library binary size"
                 (`Float (Float.of_int stats.st_size *. (1.0 /. 1024.0)));
             ]
         | exception Unix.Unix_error (ENOENT, _, _) -> []
    end
  | exception Not_found -> []
