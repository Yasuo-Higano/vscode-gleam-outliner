import gleam/io

//external fn io_format(fmt:String,params:dynamic) -> res = "gleam_erlang_ffi" "log"
external fn erl_log(fmt: String, params: dynamic) -> res =
  "erlang_ffi" "log"

external fn is_tuple(term: dynamic) -> Bool =
  "erlang" "is_tuple"

external fn tuple_to_list(term: dynamic) -> List(dyn) =
  "erlang" "tuple_to_list"

pub fn log(fmt: String, params: dynamic) {
  case is_tuple(params) {
    True -> erl_log(fmt, tuple_to_list(params))
    False -> erl_log(fmt, params)
  }
}
