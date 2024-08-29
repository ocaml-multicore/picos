let at_exit = Domain.at_exit
let recommended_domain_count = Domain.recommended_domain_count
let is_main_domain = Domain.is_main_domain

module DLS = struct
  include Domain.DLS

  let new_key init = new_key init
end
