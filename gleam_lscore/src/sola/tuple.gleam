import otp/erlang.{Tuple}

external fn erl_make_tuple(Int, x) -> Tuple =
  "erlang" "make_tuple"

external fn erl_tuple_size(Tuple) -> Int =
  "erlang" "tuple_size"

external fn erl_tuple_element(Int, Tuple) -> x =
  "erlang" "element"

external fn erl_set_tuple_element(Int, Tuple, val) -> Tuple =
  "erlang" "setelement"

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
