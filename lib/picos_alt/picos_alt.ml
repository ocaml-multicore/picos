module Branch = Branch
module Event = Event

let select : type a. a Branch.t list -> a = function
  | [] | [ _ ] -> invalid_arg "Picos_alt.select: needs at least two branches"
  | brs ->
      let module M = struct
        exception Resolve of a Branch.t
      end in
      let comp = Picos.Computation.create () in
      (* find which branch to pick *)
      let (Branch.Branch (ev, k)) =
        match
          List.iteri
            (fun i (Branch.Branch (ev, k) as branch) ->
              match ev.subscribe comp i with
              | `already_happened -> raise_notrace (M.Resolve branch)
              | `ok -> ())
            brs
        with
        | () ->
            let i : int = Picos.Computation.await comp in
            List.nth brs i
        | exception M.Resolve br -> br
      in
      Picos.Computation.cancel comp (Picos.Exn_bt.get_callstack 0 Event.Cancel);

      (* resolve the branch *)
      let x = ev.resolve () in
      k x
