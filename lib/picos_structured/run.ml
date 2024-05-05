let all actions =
  Bundle.join_after @@ fun bundle -> List.iter (Bundle.fork bundle) actions

let any actions =
  if actions == [] then Control.block ()
  else
    Bundle.join_after @@ fun bundle ->
    try
      actions
      |> List.iter @@ fun action ->
         Bundle.fork bundle @@ fun () ->
         action ();
         Bundle.terminate bundle
    with Control.Terminate -> ()
