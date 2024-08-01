open Picos_finally
open Multicore_bench

let lib_suffix = if Sys.os_type == "Win32" then "lib" else "a"

let paths =
  let lib ?(subs = []) name =
    let path base name = "lib/" ^ base ^ "/" ^ name ^ "." ^ lib_suffix in
    ( name,
      path name name
      :: List.map (fun sub -> path (name ^ "/" ^ sub) (name ^ "_" ^ sub)) subs
    )
  in
  [
    lib "picos" ~subs:[ "bootstrap"; "ocaml5"; "ocaml4" ];
    lib "picos_domain";
    lib "picos_exn_bt";
    lib "picos_fd";
    lib "picos_fifos";
    lib "picos_finally";
    lib "picos_htbl";
    lib "picos_lwt";
    lib "picos_lwt_unix";
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
      |> List.concat_map @@ fun (name, paths) ->
         match
           paths
           |> List.filter_map @@ fun path ->
              match
                let@ file =
                  finally Unix.close @@ fun () ->
                  Unix.openfile (prefix ^ "/" ^ path) [] 0
                in
                Unix.fstat file
              with
              | stats -> Some stats.st_size
              | exception Unix.Unix_error (ENOENT, _, _) -> None
         with
         | [] -> []
         | sizes ->
             let total_size = List.fold_left ( + ) 0 sizes in
             [
               Metric.make ~metric:"binary size" ~config:name ~units:"kB"
                 ~trend:`Lower_is_better ~description:"Library binary size"
                 (`Float (Float.of_int total_size *. (1.0 /. 1024.0)));
             ]
    end
  | exception Not_found -> []
