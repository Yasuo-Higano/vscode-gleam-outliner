import gleam/option

@external(erlang, "erlang_ffi", "catch_exception")
fn catch_exception(f: fn() -> result) -> Result(result, err)

@external(erlang, "erlang_ffi", "throw_exception")
fn throw_exception(err: error) -> error

pub fn catch_ex_(f: fn() -> result) -> Result(result, error) {
  Ok(f())
}

pub fn catch_ex(f: fn() -> result) -> Result(result, error) {
  catch_exception(f)
}

pub fn throw_ex(e) -> e {
  throw_exception(e)
}
