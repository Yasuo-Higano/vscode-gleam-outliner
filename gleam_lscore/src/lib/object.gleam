import lib/untyped.{Untyped}
import gleam/list
import gleam
import otp/erlang.{Atom, Term, apply_fn}
import lib/log.{log}

//pub external type Object
pub type Object =
  Atom

external fn ets_new(name: Atom, options: List(Atom)) -> Object =
  "ets" "new"

external fn insert(tab: Object, obj: a) -> Bool =
  "ets" "insert"

external fn insert_list(tab: Object, list: List(a)) -> Bool =
  "ets" "insert"

external fn lookup(tab: Object, key: String) -> List(a) =
  "ets" "lookup"

external fn foldl(fn(e, a) -> a, a, Object) -> a =
  "ets" "foldl"

external fn ets_keys(Object) -> List(String) =
  "erlang_ffi" "ets_keys"

external fn ets_to_list(Object) -> List(a) =
  "ets" "tab2list"

external fn call_method(method: fun, self: Object, params: listparam) -> a =
  "erlang_ffi" "call_method"

pub type BoxValue {
  Int(gleam.Int)
  Float(gleam.Float)
  String(gleam.String)
}

pub fn new() {
  let options: List(Atom) =
    list.map(["set", "public"], fn(e) -> Atom { erlang.atom_of(e) })
  let pid = ets_new(erlang.atom_of("gleam_object"), options)
  pid
}

pub fn new_named(name: Atom) {
  let options: List(Atom) =
    list.map(["set", "named_table", "public"], fn(e) { erlang.atom_of(e) })
  let pid = ets_new(name, options)
  pid
}

pub fn new_from_list(plist: List(a)) {
  let pid = new()
  insert_list(pid, plist)
  pid
}

pub fn set(tab: Object, key: String, val: t) -> Object {
  insert(tab, #(key, val))
  tab
}

pub fn get(tab: Object, key: String) -> t {
  let assert [#(_, val)] = lookup(tab, key)
  val
}

pub fn try_get(tab: Object, key: String) -> Result(t, String) {
  case lookup(tab, key) {
    [#(_, val)] -> Ok(val)
    _ -> Error("key not found")
  }
}

pub fn modify(tab: Object, key: String, f: fn(t) -> t) -> Object {
  let new_val = f(get(tab, key))
  set(tab, key, new_val)
}

pub fn keys(tab: Object) -> List(String) {
  ets_keys(tab)
}

pub fn to_list(tab: Object) -> List(a) {
  ets_to_list(tab)
}

pub fn call(tab: Object, method_name: String, params: a) -> b {
  let method_fn = get(tab, method_name)
  call_method(method_fn, tab, params)
}
