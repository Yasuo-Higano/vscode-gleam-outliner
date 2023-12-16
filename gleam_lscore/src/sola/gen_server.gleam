import gleam/io

pub type Atom

pub type Pid

//pub type ModuleName {
//    Sola@GenServer
//}

pub type Response(result, state) {
  Response(result: result, state: state)
}

pub type ServerResponse(result, inittype, msgtype, state, result) {
  Reply(result: result, env: Env(inittype, msgtype, state, result))
  Noreply(Env(inittype, msgtype, state, result))
}

pub type GenServer(inittype, msgtype, state, result) {
  GenServer(
    init: fn(inittype) -> state,
    call: fn(msgtype, state) -> Response(result, state),
    cast: fn(msgtype, state) -> Response(result, state),
    info: fn(msgtype, state) -> Response(result, state),
  )
}

pub type Env(inittype, msgtype, state, result) {
  Env(def: GenServer(inittype, msgtype, state, result), state: state)
}

@external(erlang, "io_lib", "format")
pub fn format(fmt: String, param: a) -> String

@external(erlang, "gen_server", "start_link")
pub fn start_link_(a: a, b: b, c: c) -> Result(Pid, err)

@external(erlang, "erlang", "binary_to_atom")
pub fn string_to_atom(str: String) -> Result(Atom, err)

@external(erlang, "gen_server", "call")
pub fn call(pid: Pid, msg: dyn) -> Result(a, b)

@external(erlang, "gen_server", "cast")
pub fn cast(pid: Pid, msg: dyn) -> Result(a, b)

@external(erlang, "erlang", "tuple_to_list")
pub fn tuple_to_list(in: a) -> b

pub fn start_link(
  def: GenServer(inittype, msgtype, state, result),
  initparam: inittype,
) -> Result(Pid, err) {
  let atom = string_to_atom("sola@gen_server")
  start_link_(atom, #(def, initparam), [])
}

pub fn init(initarg) -> Result(Env(inittype, msgtype, state, result), a) {
  let #(def, initparam) = initarg
  Ok(Env(def: def, state: def.init(initparam)))
}

pub fn handle_call(
  msg: msgtype,
  from: Pid,
  env: Env(inittype, msgtype, state, result),
) -> ServerResponse(result, inittype, msgtype, state, result) {
  let res = env.def.call(msg, env.state)
  Reply(res.result, Env(..env, state: res.state))
}

pub fn handle_cast(
  msg: msgtype,
  env: Env(inittype, msgtype, state, result),
) -> ServerResponse(result, inittype, msgtype, state, result) {
  let res = env.def.cast(msg, env.state)
  Noreply(Env(..env, state: res.state))
}

pub fn handle_info(
  msg: msgtype,
  env: Env(inittype, msgtype, state, result),
) -> ServerResponse(result, inittype, msgtype, state, result) {
  let res = env.def.info(msg, env.state)
  Noreply(Env(..env, state: res.state))
}
