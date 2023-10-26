let at_exit = Stdlib.at_exit

module DLS = struct
  type 'a key = 'a ref

  let new_key default = ref (default ())
  let get = ( ! )
  let set = ( := )
end
