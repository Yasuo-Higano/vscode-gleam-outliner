import lib/log.{log}

external fn pp(a) -> String =
  "ffi_gleam_pp" "pp"

pub fn pprint(a) {
  pp(a)
}

pub fn pprint_out(a) {
  log("~s", [pp(a)])
}
