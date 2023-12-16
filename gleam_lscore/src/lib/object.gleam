import lib/untyped
import gleam/list
import gleam
import otp/erlang.{type Atom}
import lib/log.{log}

//pub external type Object
pub type Object =
  Atom

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(Atom)) -> Object

@external(erlang, "ets", "insert")
fn insert(tab: Object, obj: a) -> Bool

@external(erlang, "ets", "insert")
fn insert_list(tab: Object, list: List(a)) -> Bool

@external(erlang, "ets", "lookup")
fn lookup(tab: Object, key: String) -> List(a)

@external(erlang, "ets", "foldl")
fn foldl(f: fn(e, a) -> a, a: a, o: Object) -> a

@external(erlang, "erlang_ffi", "ets_keys")
fn ets_keys(o: Object) -> List(String)

@external(erlang, "ets", "tab2list")
fn ets_to_list(o: Object) -> List(a)

@external(erlang, "erlang_ffi", "call_method")
fn call_method(method: fun, self: Object, params: listparam) -> a

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
