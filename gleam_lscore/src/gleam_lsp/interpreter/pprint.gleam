import lib/log.{log}

@external(erlang, "ffi_gleam_pp", "pp")
fn pp(a: a) -> String

pub fn pprint(a: a) {
  pp(a)
}

pub fn pprint_out(a: a) {
  log("~s", [pp(a)])
}
