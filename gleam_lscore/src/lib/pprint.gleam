import gleam
import lib/symbol.{symbol_of}
import lib/untyped.{type Untyped, untyped}
import gleam/list
import gleam/string
import otp/erlang.{list_to_binary}

@external(erlang, "io_lib", "format")
pub fn format_(fmt: String, param: List(anything)) -> List(Int)

@external(erlang, "pprintreg", "start")
pub fn pprintreg_start() -> Untyped

@external(erlang, "pprintreg", "apply_ppfn")
pub fn apply_ppfn(x: x) -> String

@external(erlang, "pprintreg", "register_ppfn")
pub fn register_ppfn(name: name, func: fn(x) -> String) -> Untyped

pub type PPrn(x) {
  Str(gleam.String)
  Int(gleam.Int)
  Float(gleam.Float)
  List(gleam.List(PPrn(x)))
  Dyn(x)
}

pub fn start() {
  pprintreg_start()
}

//fn pprint_(src: List(PPrn(x)), acc: List(String)) -> String {
//  case src {
//    [] ->
//      list.reverse(acc)
//      |> string.concat
//    _ -> {
//      let #(str, rest) = case src {
//        [Str(val), ..rest] -> #(format("~s", [val]), rest)
//        [Int(val), ..rest] -> #(format("~p", [val]), rest)
//        [Float(val), ..rest] -> #(format("~p", [val]), rest)
//        [Dyn(val), ..rest] -> #(apply_ppfn(val), rest)
//      }
//      pprint_(rest, [str, ..acc])
//    }
//  }
//}
//
//pub fn pprintfmt(src: List(PPrn(x))) {
//  pprint_(src, [])
//}
pub fn register(typename: String, func: fn(x) -> String) {
  register_ppfn(symbol_of(typename), func)
}

pub fn format(fmt: String, param: List(anything)) -> String {
  list_to_binary(format_(fmt, param))
}
