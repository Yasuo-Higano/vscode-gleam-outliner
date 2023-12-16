import gleam/option.{type Option}

pub type Term

pub type Atom

pub type Pid

pub type Tuple

pub type Unit =
  Option(List(Atom))

@external(erlang, "erlang", "byte_size")
fn byte_size(str: String) -> Int

@external(erlang, "erlang", "binary_to_atom")
pub fn string_to_atom(str: String) -> Atom

@external(erlang, "erlang", "atom_to_binary")
fn atom_to_string(atom: Atom) -> String

@external(erlang, "erlang", "list_to_binary")
pub fn list_to_binary(str: List(a)) -> String

@external(erlang, "erlang", "tuple_to_list")
pub fn tuple_to_list(in: a) -> b

@external(erlang, "erlang", "list_to_tuple")
pub fn list_to_tuple(in: a) -> b

@external(erlang, "erlang", "tuple_size")
pub fn tuple_size(a: a) -> Int

@external(erlang, "erlang", "trunc")
pub fn trunc(val: Float) -> Int

@external(erlang, "erlang", "float")
pub fn float(val: Int) -> Float

@external(erlang, "erlang", "apply")
pub fn apply(module: Atom, f: Atom, args: list_of_term) -> Term

@external(erlang, "erlang", "apply")
pub fn apply_fn(f: fun, args: list_of_term) -> Term

@external(erlang, "erlang", "spawn")
pub fn spawn(f: fn() -> Unit) -> Pid

pub fn atom_of(str: String) -> Atom {
  string_to_atom(str)
}
