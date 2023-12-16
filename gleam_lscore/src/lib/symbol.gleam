//import type_printer.{Atom}
//pub external type 
pub type Atom

//pub external fn symbol_of(name:String) -> Atom = "gleam_erlang_ffi" "symbol_of"
@external(erlang, "erlang", "binary_to_atom")
pub fn symbol_of(name: String) -> Atom
