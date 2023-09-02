import gleam/option.{Option, Some, None}

external fn catch_exception(f:fn()->result) -> Result(result, err) = "erlang_ffi" "catch_exception"
external fn throw_exception(err:error) -> error = "erlang_ffi" "throw_exception"

pub fn catch_ex_( f:fn()->result ) -> Result(result, error) {
    Ok( f() )
}

pub fn catch_ex( f:fn()->result ) -> Result(result, error) {
    catch_exception(f)
}

pub fn throw_ex(e) -> e {
    throw_exception(e)
}