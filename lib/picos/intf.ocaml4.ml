module type Trigger = sig
  type t
end

module type Computation = sig
  type _ t
end

module type Fiber = sig
  type t
  type maybe
  type _ computation
end
