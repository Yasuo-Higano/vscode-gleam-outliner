@external(erlang, "erlang", "list_to_binary")
fn list_to_binary(l: dynamec) -> String

@external(erlang, "lists", "flatten")
fn list_flatten(term: dynamic) -> List(dyn)

@external(erlang, "io_lib", "format")
fn format_(fmt: String, params: dynamic) -> String

pub fn format(fmt: String, params: dynamic) -> String {
  list_to_binary(list_flatten(format_(fmt, params)))
}
