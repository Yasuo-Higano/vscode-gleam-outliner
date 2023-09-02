import gleam/option.{None, Option, Some}

pub external type Term

pub external type Atom

pub external type Pid

pub external type Tuple

pub type Unit =
  Option(List(Atom))

pub external fn byte_size(str: String) -> Int =
  "erlang" "byte_size"

pub external fn string_to_atom(str: String) -> Atom =
  "erlang" "binary_to_atom"

pub external fn atom_to_string(atom: Atom) -> String =
  "erlang" "atom_to_binary"

pub external fn list_to_binary(str: List(a)) -> String =
  "erlang" "list_to_binary"

pub external fn tuple_to_list(in: a) -> b =
  "erlang" "tuple_to_list"

pub external fn list_to_tuple(in: a) -> b =
  "erlang" "list_to_tuple"

pub external fn tuple_size(a) -> Int =
  "erlang" "tuple_size"

pub external fn trunc(val: Float) -> Int =
  "erlang" "trunc"

pub external fn float(val: Int) -> Float =
  "erlang" "float"

pub external fn apply(module: Atom, f: Atom, args: list_of_term) -> Term =
  "erlang" "apply"

pub external fn apply_fn(f: fun, args: list_of_term) -> Term =
  "erlang" "apply"

pub external fn spawn(f: fn() -> Unit) -> Pid =
  "erlang" "spawn"

pub fn atom_of(str: String) -> Atom {
  string_to_atom(str)
}
