///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

// command log
// new(TargetPID)
// record(Cmd) -> Int
// replay(Int)
// save
// load
//
// record( NewDoc())
// add(Doc)
// List(Record(inf(timestamp), document))
import lib/log.{log}
import gleam/list
import gleam/option.{None, Option, Some}
import otp/erlang.{Atom, Pid, Unit, string_to_atom}

//import gleam.{Tuple}
pub type Record(data) {
  Record(data: data)
}

pub fn new(plug: Plug(props, data)) -> Pid {
  let assert Ok(pid) = start_link(plug)
  pid
}

pub fn append(pid: Pid, record: Record(data)) -> Int {
  let assert RecordID(n) = call(pid, Append(record))
  n
}

pub fn get(pid: Pid, record_id: Int) -> Record(data) {
  let assert RecordData(record) = call(pid, Get(record_id))
  record
}

pub external fn start_link_(a, b, c) -> Result(Pid, err) =
  "gen_server" "start_link"

pub external fn call(pid: Pid, msg: dyn) -> res =
  "gen_server" "call"

pub type GenServerResponse(resp, state) {
  Reply(ret: resp, state: state)
  Noreply(state: state)
}

pub type Plug(props, data) {
  Plug(
    property: props,
    load: fn(props) -> Result(State(data), String),
    save: fn(props, data) -> Unit,
  )
}

pub type DB(data) =
  List(Record(data))

pub type InitParam(props, data) {
  InitParam(plug: Plug(props, data))
}

pub type State(data) {
  State(db: DB(data))
}

pub type Env(props, data) {
  Env(plug: Plug(props, data), state: State(data))
}

pub type Method(data) {
  Append(record: Record(data))
  Get(record_id: Int)
}

pub type Resp(data) {
  RecordData(record: Record(data))
  RecordID(Int)
}

pub fn start_link(plug: Plug(props, data)) -> Result(Pid, err) {
  let module_atom = string_to_atom("sola@command_log")
  // ?MODULE 
  start_link_(module_atom, InitParam(plug), [])
}

//pub fn init([initial_page:Record(data), plug:Plug(data)]) -> Result( State(data), err) {
pub fn init(init_param: InitParam(props, data)) -> Result(Env(props, data), err) {
  let InitParam(plug) = init_param
  case plug.load(plug.property) {
    Ok(state) -> Ok(Env(plug: plug, state: state))
    _ -> {
      let db: List(Record(data)) = []
      Ok(Env(plug: plug, state: State(db: db)))
    }
  }
}

pub fn handle_call(
  msg: Method(data),
  _from,
  env: Env(props, data),
) -> GenServerResponse(Resp(data), Env(props, data)) {
  case msg {
    Get(record_id) -> {
      //assert Ok([record, .._]) = map.get(state.db, record_id)
      let assert Ok(record) = list.at(env.state.db, record_id)
      Reply(RecordData(record), env)
    }
    Append(record) -> {
      let sz = list.length(env.state.db)
      let new_state =
        Env(..env, state: State(..env.state, db: [record, ..env.state.db]))
      Reply(RecordID(sz), new_state)
    }
  }
}

type MyCommand {
  MyCommand(title: String, body: String)
}

type MyProperty {
  MyProperty(filename: String)
}

pub fn t1() {
  let property = MyProperty("/aoei.data")
  let plug =
    Plug(
      property: property,
      load: fn(p: MyProperty) { Error("err") },
      save: fn(p: MyProperty, state: State(MyCommand)) -> Unit { None },
    )

  let pid = new(plug)
  append(pid, Record(MyCommand(title: "iititle", body: "Uho IIOTOKO!")))
  let record = get(pid, 0)
  log("record 0 = ~p", [record])
  append(
    pid,
    Record(MyCommand(title: "** iititle** ", body: "#### Uho IIOTOKO! ####")),
  )
  let page_ = get(pid, 0)
  log("record 0 = ~p", [page_])
}

//pub fn t2() {
//    let pid = new( Record(MyDocument2( title:"iititle", body:"Uho IIOTOKO!", ps:"ABABABA"), MyDocInfo(timestamp: 100) ) )
//    let record = get(pid, 0)
//    log("record 0 = ~p",[record])
//    update(pid, 0, Record(MyDocument2( title:"** iititle** ", body:"#### Uho IIOTOKO! ####", ps:"***Ababababa******"), MyDocInfo(timestamp: 200) ) )
//    let page_ = get(pid, 0)
//    log("record 0 = ~p",[page_])
//}
pub fn test() {
  log("START ------------", [])
  t1()
  //t2()
}
