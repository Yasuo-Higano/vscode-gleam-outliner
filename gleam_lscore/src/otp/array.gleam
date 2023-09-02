pub external type Array(t)

pub external fn fix(Array(t)) -> Array(t) = "array" "fix"
pub external fn foldl(fn(Int,t,acc)->acc,acc,Array(t)) -> acc = "array" "foldl"
pub external fn get(Int,Array(t)) -> t = "array" "get"
pub external fn new_fixed(Int) -> Array(t) = "array" "new"
pub external fn new() -> Array(t) = "array" "new"
pub external fn set(Int,t,Array(t)) -> Array(t) = "array" "set"
pub external fn size(Array(t)) -> Int = "array" "size"
pub external fn to_list(Array(t)) -> List(t) = "array" "to_list"
pub external fn from_list(List(t)) -> Array(t) = "array" "from_list"
