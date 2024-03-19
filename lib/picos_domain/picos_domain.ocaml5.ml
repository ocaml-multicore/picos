let at_exit = Domain.at_exit

module DLS = struct
  include Domain.DLS

  let new_key init = new_key init
end
