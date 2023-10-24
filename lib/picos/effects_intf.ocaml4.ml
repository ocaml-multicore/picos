module type Exn_bt = sig
  type t
end

module type Trigger = sig
  type _ t
  type exn_bt
end

module type Computation = sig
  type _ as_cancelable
  type exn_bt
end

module type Fiber = sig
  type _ t
  type _ as_cancelable
end
