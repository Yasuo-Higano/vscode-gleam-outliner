pub type Array(t)

@external(erlang, "array", "fix")
pub fn fix(a: Array(t)) -> Array(t)

@external(erlang, "array", "foldl")
pub fn foldl(f: fn(Int, t, acc) -> acc, acc: acc, a: Array(t)) -> acc

@external(erlang, "array", "get")
pub fn get(i: Int, a: Array(t)) -> t

@external(erlang, "array", "new")
pub fn new_fixed(i: Int) -> Array(t)

@external(erlang, "array", "new")
pub fn new() -> Array(t)

@external(erlang, "array", "set")
pub fn set(i: Int, t: t, a: Array(t)) -> Array(t)

@external(erlang, "array", "size")
pub fn size(a: Array(t)) -> Int

@external(erlang, "array", "to_list")
pub fn to_list(a: Array(t)) -> List(t)

@external(erlang, "array", "from_list")
pub fn from_list(l: List(t)) -> Array(t)
