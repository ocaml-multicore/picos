let report_leaks = ref true

module File_descr = struct
  type t = Unix.file_descr

  let equal : t -> t -> bool = ( == )

  let hash (fd : t) =
    if Obj.is_int (Obj.repr fd) then Obj.magic fd else Hashtbl.hash fd

  let dispose = Unix.close
end

include Picos_aux_rc.Make (File_descr) ()

let () =
  Stdlib.at_exit @@ fun () ->
  if !report_leaks then
    infos ()
    |> Seq.iter @@ fun info ->
       if 0 < info.count && info.dispose then begin
         if Obj.is_int (Obj.repr info.resource) then
           Printf.eprintf "Leaked file descriptor (%d).\n%!"
             (Obj.magic info.resource)
         else Printf.eprintf "Leaked file descriptor.\n%!";
         match Printexc.backtrace_slots info.bt with
         | None -> ()
         | Some slots ->
             slots
             |> Array.iteri @@ fun i slot ->
                if i <> 0 then
                  Printexc.Slot.format i slot
                  |> Option.iter (Printf.eprintf "  %s\n%!")
       end
