import otp/erlang.{type Tuple}

@external(erlang, "erlang", "make_tuple")
pub fn erl_make_tuple(i: Int, x: x) -> Tuple

@external(erlang, "erlang", "tuple_size")
pub fn erl_tuple_size(t: Tuple) -> Int

@external(erlang, "erlang", "element")
pub fn erl_tuple_element(i: Int, t: Tuple) -> x

@external(erlang, "erlang", "setelement")
pub fn erl_set_tuple_element(i: Int, t: Tuple, v: val) -> Tuple

pub fn make(n: Int) -> Tuple {
  erl_make_tuple(n, 0)
}

pub fn size(t: Tuple) -> Int {
  erl_tuple_size(t)
}

pub fn at(t: Tuple, idx: Int) -> x {
  erl_tuple_element(idx, t)
}

pub fn set_at(t: Tuple, idx: Int, val: val) -> Tuple {
  erl_set_tuple_element(idx, t, val)
}
