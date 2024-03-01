module type Exn_bt = sig
  type t
end

module type Trigger = sig
  type t
  type exn_bt
end

module type Computation = sig
  type _ t
  type exn_bt
end

module type Fiber = sig
  type t
  type _ computation
end
