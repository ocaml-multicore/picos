open Picos_domain

let is_main_thread = is_main_domain

module TLS = struct
  type 'a t = 'a DLS.key

  exception Not_set

  let create () = DLS.new_key @@ fun () -> raise_notrace Not_set
  let get_exn = DLS.get
  let set = DLS.set
end
