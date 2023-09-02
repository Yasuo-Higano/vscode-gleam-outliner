//import type_printer.{Atom}
//pub external type 
pub external type Atom 

//pub external fn symbol_of(name:String) -> Atom = "gleam_erlang_ffi" "symbol_of"
pub external fn symbol_of(name:String) -> Atom = "erlang" "binary_to_atom"