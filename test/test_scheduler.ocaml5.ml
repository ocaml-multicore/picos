open Picos_structured.Finally

let use_randos = Random.bool (Random.self_init ()) && false

let run ?(max_domains = 1) ?forbid main =
  if use_randos then
    let context = Picos_randos.context () in
    let rec spawn n =
      if n <= 1 then Picos_randos.run ~context ?forbid main
      else
        let@ _ =
          finally Domain.join @@ fun () ->
          try
            Domain.spawn @@ fun () -> Picos_randos.runner_on_this_thread context
          with exn ->
            Picos_randos.run ~context Fun.id;
            raise exn
        in
        spawn (n - 1)
    in
    spawn (Int.min max_domains (Domain.recommended_domain_count ()))
  else Picos_fifos.run ?forbid main
