import otp/erlang.{Atom}
import sola/exception

pub external fn get2(key: Atom, default_val: a) -> a =
  "persistent_term" "get"

pub external fn put(key: Atom, value: a) -> b =
  "persistent_term" "put"

pub fn modify(key: Atom, default_value: a, f: fn(a) -> a) -> a {
  let newval = f(get2(key, default_value))
  put(key, newval)
  newval
}

pub fn increment(key: Atom) -> Int {
  let newval = get2(key, 0) + 1
  put(key, newval)
  newval
}

pub fn exists(key: Atom) -> Bool {
  let undefined = erlang.atom_of("undefined")
  let res = exception.catch_ex(fn() { get2(key, undefined) != undefined })
  case res {
    Ok(f) -> f
    _ -> False
  }
}
