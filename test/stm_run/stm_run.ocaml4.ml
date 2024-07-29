include Intf

let count =
  let factor b = if b then 10 else 1 in
  factor (64 <= Sys.word_size) * factor (Sys.backend_type = Native) * 10

let run ?(verbose = true) ?(count = count) ?(budgetf = 60.0) ~name ?make_domain
    (module Spec : STM.Spec) =
  let module Seq = STM_sequential.Make (Spec) in
  let module Con = STM_thread.Make (Spec) [@alert "-experimental"] in
  Util.run_with_budget ~budgetf ~count @@ fun count ->
  [
    [ Seq.agree_test ~count ~name:(name ^ " sequential") ];
    (match make_domain with
    | None -> [ Con.agree_test_conc ~count ~name:(name ^ " concurrent") ]
    | Some _ -> []);
  ]
  |> List.concat
  |> QCheck_base_runner.run_tests ~verbose
