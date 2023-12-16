import gleam/io
import lib/log.{log}
import lib/untyped.{untyped}
import gleam

pub type Void

pub type Atom

pub type Pid

pub type Atoms {
  Normal
  TrapExit
}

pub type Msg(t) {
  GetValue
  UpdateValue(fn(t) -> t)
  Delete
}

pub type Response(t) {
  Reply(ret: Result(t, String), state: t)
  Noreply(state: t)
  Stop(mode: Atoms, state: t)
}

pub type Reference(t) {
  Reference(pid: Pid, dummy: t)
}

//Reference(pid:Pid)
pub fn make_reference(val: a) -> Reference(a) {
  let assert Ok(ref) = start_link(val)
  Reference(ref, val)
}

pub fn dereference(ref: Reference(a)) -> a {
  let Reference(pid, a) = ref
  let assert Ok(val) = call(pid, GetValue)
  val
}

// returns the old value
pub fn update_reference(ref: Reference(a), f: fn(a) -> a) -> a {
  let Reference(pid, a) = ref
  let assert Ok(val) = call(pid, UpdateValue(f))
  val
}

pub fn delete_reference(ref: Reference(a)) -> a {
  let Reference(pid, a) = ref
  let assert Ok(val) = call(pid, GetValue)
  call(pid, Delete)
  //stop(pid)
  val
}

@external(erlang, "io_lib", "format")
fn format(fmt: String, param: a) -> String

@external(erlang, "gen_server", "start_link")
fn start_link_(a: a, b: b, c: c) -> Result(Pid, err)

@external(erlang, "gen_server", "start")
fn start_(a: a, b: b, c: c) -> Result(Pid, err)

@external(erlang, "erlang", "binary_to_atom")
fn string_to_atom(str: String) -> Result(Atom, err)

@external(erlang, "gen_server", "call")
fn call(pid: Pid, msg: Msg(t)) -> Result(t, err)

@external(erlang, "gen_server", "stop")
fn stop(pid: Pid) -> Result(t, err)

//external fn cast(pid:Pid, msg:dyn) -> Result(a,b) = "gen_server" "cast"
@external(erlang, "erlang", "tuple_to_list")
fn tuple_to_list(in: a) -> b

@external(erlang, "erlang", "process_flag")
fn process_flag(a: Atoms, b: Bool) -> Void

fn start_link(val: tval) -> Result(Pid, err) {
  let module_atom = string_to_atom("gleam@reference")
  //start_link_(module_atom, val, [])
  start_(module_atom, val, [])
}

pub fn init(state: tval) -> Result(tval, error) {
  process_flag(TrapExit, True)
  Ok(state)
}

pub fn handle_call(msg: Msg(tval), _from: Pid, val: tval) -> Response(tval) {
  //io.println( format("call:[[ ~p ]] (~p)",[ untyped(msg),untyped(val)] ) )
  case msg {
    GetValue -> Reply(Ok(val), val)
    UpdateValue(f) -> {
      let uval = f(val)
      Reply(Ok(uval), uval)
    }
    Delete -> Stop(Normal, val)
    _ -> Reply(Error("undefined"), val)
  }
}

pub fn terminate(state, val) {
  log("TERMINATE MSG ~p", [state])
  string_to_atom("ok")
}

pub fn test() {
  let ref = make_reference(123)
  log("ref value = ~p", [dereference(ref)])
  let uval = update_reference(ref, fn(i) { i + 1 })
  log("ref value = ~p", [uval])
  let uval2 = update_reference(ref, fn(i) { i + 1 })
  log("ref value = ~p", [uval2])

  //delete_reference(ref)
  let ref2 = make_reference("abc")
  log("ref2 value = ~p", [dereference(ref2)])
}
