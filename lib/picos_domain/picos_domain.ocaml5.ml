let at_exit = Domain.at_exit
let recommended_domain_count = Domain.recommended_domain_count

module DLS = struct
  include Domain.DLS

  let new_key init = new_key init
end
