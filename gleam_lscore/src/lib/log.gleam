import gleam/io

//external fn io_format(fmt:String,params:dynamic) -> res = "gleam_erlang_ffi" "log"
@external(erlang, "erlang_ffi", "log")
fn erl_log(fmt: String, params: dynamic) -> res

@external(erlang, "erlang", "is_tuple")
fn is_tuple(term: dynamic) -> Bool

@external(erlang, "erlang", "tuple_to_list")
fn tuple_to_list(term: dynamic) -> List(dyn)

pub fn log(fmt: String, params: dynamic) {
  case is_tuple(params) {
    True -> erl_log(fmt, tuple_to_list(params))
    False -> erl_log(fmt, params)
  }
}
