
external fn list_to_binary(l:dynamec) -> String = "erlang" "list_to_binary"
external fn list_flatten(term:dynamic) -> List(dyn) = "lists" "flatten"

external fn format_(fmt:String,params:dynamic) -> String = "io_lib" "format"

pub fn format(fmt:String,params:dynamic) -> String {
    list_to_binary( list_flatten( format_(fmt,params) ) )
}