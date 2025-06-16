type _ tdt =
  | Empty : [> `Empty ] tdt
  | Range : {
      mutable lo : int;
      hi : int;
      parent : [ `Empty | `Range ] tdt;
    }
      -> [> `Range ] tdt

let[@poll error] cas_lo (Range r : [ `Range ] tdt) before after =
  r.lo == before
  && begin
       r.lo <- after;
       true
     end

let rec for_out t (Range r as range : [ `Range ] tdt) action =
  let lo_before = r.lo in
  let n = r.hi - lo_before in
  if 0 < n then begin
    if Bundle.is_running t then begin
      let lo_after = lo_before + 1 in
      if cas_lo range lo_before lo_after then begin
        try action lo_before
        with exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
      end;
      for_out t range action
    end
  end
  else
    match r.parent with
    | Empty -> ()
    | Range _ as range -> for_out t range action

let rec for_in t (Range r as range : [ `Range ] tdt) action =
  let lo_before = r.lo in
  let n = r.hi - lo_before in
  if n <= 1 then for_out t range action
  else
    let lo_after = lo_before + (n asr 1) in
    if cas_lo range lo_before lo_after then begin
      Bundle.fork t (fun () -> for_in t range action);
      let child = Range { lo = lo_before; hi = lo_after; parent = range } in
      for_in t child action
    end
    else for_in t range action

let for_n ?on_terminate n action =
  if 0 < n then
    if n = 1 then
      try action 0
      with
      | Control.Terminate when Bundle.on_terminate on_terminate == `Ignore ->
        ()
    else
      let range = Range { lo = 0; hi = n; parent = Empty } in
      Bundle.join_after ?on_terminate @@ fun t -> for_in t range action
