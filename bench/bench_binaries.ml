open Picos_std_finally
open Multicore_bench

let lib_suffix = if Sys.os_type == "Win32" then "lib" else "a"

let paths =
  let lib ?(subs = []) name =
    let path base name = "lib/" ^ base ^ "/" ^ name ^ "." ^ lib_suffix in
    ( name,
      path name (String.map (function '.' -> '_' | c -> c) name)
      :: List.map (fun sub -> path (name ^ "/" ^ sub) (name ^ "_" ^ sub)) subs
    )
  in
  [
    lib "picos";
    lib "picos.domain";
    lib "picos.thread";
    lib "picos_aux.htbl";
    lib "picos_aux.mpmcq";
    lib "picos_aux.mpscq";
    lib "picos_aux.rc";
    lib "picos_lwt";
    lib "picos_lwt.unix";
    lib "picos_mux.fifo";
    lib "picos_mux.multififo";
    lib "picos_mux.random";
    lib "picos_mux.thread";
    lib "picos_std.awaitable";
    lib "picos_std.event";
    lib "picos_std.finally";
    lib "picos_std.structured";
    lib "picos_std.sync";
    lib "picos_io";
    lib "picos_io.fd";
    lib "picos_io.select";
    lib "picos_io_cohttp";
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
