import otp/erlang
import gleam
import lib/log as liblog
import gleam/io
import gleam/list
import gleam/string
import gleam/map.{type Map}
import gleam/option.{type Option, None, Some}
import gleam/dynamic
import sola/exception
import sola/json.{
  type JPair, type Json, JInt, JList, JNull, JObject, JPair, JStr,
}
import gleam_lsp/interpreter/token
import gleam_lsp/interpreter/lexer
import gleam_lsp/interpreter/itype.{type Token2}
import gleam_lsp/interpreter/fuzzy_doc_symbolizer.{
  type DocSymbol2, Constructor, DefConst, DefExternalFun, DefExternalType,
  DefType, DefTypeAlias, Defun, DocSymbol2, ParentWithChildren,
}

type StandardIo {
  StandardIo
  StandardError
}

pub type Atom

pub type Pid

pub type DocData {
  DocData(src: String, token: List(Token2), symbol: List(DocSymbol2))
}

pub type State {
  State(files: Map(String, DocData))
}

fn new_state() {
  State(map.new())
}

pub type Response {
  Reply(ret: Option(String), state: State)
  Noreply(state: State)
}

pub fn start() -> Pid {
  let assert Ok(pid) = start_gen_server()
  pid
}

pub fn handle_message(pid: Pid, msg: String) -> Option(String) {
  call(pid, msg, 3000)
}

@external(erlang, "gen_server", "start_link")
pub fn start_(a: a, b: b, c: c) -> Result(Pid, err)

@external(erlang, "erlang", "binary_to_atom")
pub fn string_to_atom(str: String) -> Result(Atom, err)

@external(erlang, "gen_server", "call")
pub fn call(pid: Pid, msg: dyn, timeout: Int) -> Option(String)

@external(erlang, "gen_server", "cast")
pub fn cast(pid: Pid, msg: dyn) -> String

pub fn init_module() -> Nil {
  log("# gleam_lsp:init_module", [])
}

fn log(msg: String, args: List(dyn)) {
  liblog.log("# gleam " <> msg, args)
}

pub fn start_gen_server() -> Result(Pid, err) {
  let global_atom = string_to_atom("global")
  let module_atom = string_to_atom("gleam_lsp@languageserver@server")
  //start_(#(global_atom, module_atom), [], [])
  log("start_gen_server: ~p", [module_atom])
  start_(module_atom, [], [])
}

pub fn init(args: dyn) -> Result(State, a) {
  Ok(new_state())
}

//{ "jsonrpc": "2.0", "id": ID, "result": RESULT }
fn make_response(opt_id: Option(Json), msg: List(JPair)) {
  let l1 = [JPair("jsonrpc", JStr("2.0"))]
  let l2 =
    list.append(l1, case opt_id {
      Some(id) -> [JPair("id", id)]
      None -> []
    })

  json.encode(JObject(list.append(l2, msg)))
}

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#range
type Pos {
  Pos(Int, Int)
}

fn pos(p: Pos) {
  let Pos(line, character) = p
  JObject([JPair("line", JInt(line)), JPair("character", JInt(character))])
}

fn range(start: Pos, end: Pos) {
  JObject([JPair("start", pos(start)), JPair("end", pos(end))])
}

fn emit_docsymbol(e: DocSymbol2) -> Json {
  let DocSymbol2(sym, r) = e
  case sym {
    ParentWithChildren(parent, children) ->
      emit_docsymbol_(
        parent,
        JList(list.map(children, fn(e) { emit_docsymbol(e) })),
      )
    _ -> emit_docsymbol_(e, JNull)
  }
}

// https://raw.githubusercontent.com/microsoft/vscode/main/src/vscode-dts/vscode.d.ts
// SymbolKind
fn emit_docsymbol_(e: DocSymbol2, children: Json) -> Json {
  let DocSymbol2(sym, r) = e
  //let r = Range(r_.sl + 1, r_.sc + 1, r_.el + 1, r_.ec + 1)
  let #(name, kind) = case sym {
    DefConst(public, name) -> #(name, 14)
    Defun(public, name) -> #(name, 12)
    DefType(public, name) -> #(name, 10)
    DefTypeAlias(public, name) -> #(name, 5)
    Constructor(public, name) -> #(name, 22)
    DefExternalFun(public, name) -> #(name, 6)
    DefExternalType(public, name) -> #(name, 23)
    _ -> #("unknown", 0)
  }
  let symbol_len = string.length(name)
  let lobj = [
    JPair("name", JStr(name)),
    JPair("kind", JInt(kind)),
    JPair("selectionRange", range(Pos(r.sl, r.sc), Pos(r.sl, r.sc + symbol_len)),
    ),
    JPair("range", range(Pos(r.sl, r.sc), Pos(r.el, r.ec))),
  ]
  let lobj2 =
    list.append(lobj, case children {
      JList(_) -> [JPair("children", children)]
      _ -> []
    })
  //log("LOBJ2 = ~p", [lobj2])
  JObject(lobj2)
}

fn build_docsym(doc: DocData) {
  //log("BUILD DOCSYM ~p", [doc.symbol])
  exception.catch_ex(fn() {
    let symbols = doc.symbol
    list.map(symbols, fn(e) { emit_docsymbol(e) })
  })
}

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#documentSymbol
fn provide_symbol_information(state: State, opt_id: Option(Json), juri: Json) {
  log("provide_symbol_information uri: ~p", [juri])

  //  let syminf =
  //    JObject([
  //      JPair("name", JStr("add")),
  //      JPair("kind", JInt(6)),
  //      JPair("range", range(Pos(0, 7), Pos(0, 13))),
  //      JPair("selectionRange", range(Pos(0, 7), Pos(0, 13))),
  //    ])
  let assert JStr(uri) = juri
  //log("state.files = ~p", [state.files])
  let doc = map.get(state.files, uri)
  case doc {
    Ok(doc) ->
      case build_docsym(doc) {
        Ok(syminf) -> {
          let params = [JPair("result", JList(syminf))]
          #(Some(make_response(opt_id, params)), state)
        }
        _ -> #(None, state)
      }
    _ -> #(None, state)
  }
}

fn compile(state: State, j_uri: Json, j_text: Json) {
  log("compile uri: ~p", [j_uri])
  //log("text: ~p", [j_text])
  let assert JStr(uri) = j_uri
  let assert JStr(src) = j_text

  case
    exception.catch_ex(fn() {
      //log("**** LEXER", [])
      let tokens = lexer.parse(src)
      //log("**** PARSER", [])
      let symbols = fuzzy_doc_symbolizer.parse(tokens)
      //log("**** FUZZY SYMBOLIZER\n~p", [symbols])
      let doc = DocData(src: src, token: tokens, symbol: symbols)
      let new_state = State(..state, files: map.insert(state.files, uri, doc))
    })
  {
    Ok(new_state) -> {
      log("compile ok: ~s", [uri])
      #(None, new_state)
    }

    _ -> {
      log("compile failed: ~s", [uri])
      #(None, state)
    }
  }
}

pub fn handle_call(msg: String, from: Pid, state: State) -> Response {
  let jobj = json.decode(msg)
  //log("\n\njobj = ~p", [jobj])
  let method = json.string_of(json.get(jobj, "method"))
  //log("//\n// method = ~p\n//", [method])
  let xid = json.get(jobj, "id")
  //log("id = ~p", [xid])

  let id = case xid {
    JNull -> None
    _ -> Some(xid)
  }

  //log("id2 = ~p", [id])
  // https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#serverCapabilities
  let #(resp, new_state) = case method {
    "initialize" -> #(
      Some(
        make_response(id, [
          JPair(
            "result",
            JObject([
              JPair(
                "capabilities",
                JObject([
                  JPair("textDocumentSync", JInt(1)),
                  JPair("documentSymbolProvider", JInt(1)),
                ]),
              ),
            ]),
          ),
        ]),
      ),
      state,
    )
    "initialized" -> #(None, state)

    "textDocument/didOpen" -> {
      let uri = json.getp(jobj, ["params", "textDocument", "uri"])
      let text = json.getp(jobj, ["params", "textDocument", "text"])
      compile(state, uri, text)
    }

    "textDocument/didChange" -> {
      let uri = json.getp(jobj, ["params", "textDocument", "uri"])
      let assert JList(changes) = json.getp(jobj, ["params", "contentChanges"])
      //list.each(changes, fn(e) { compile(state, uri, json.get(e, "text")) })
      //list.each(changes, fn(e) { log("text = ~ts", [json.get(e, "text")]) })
      let new_state =
        list.fold(changes, state, fn(st, e) {
          //log("text = ~ts", [json.get_nt_p(e, "text")])
          let #(_, ret_state) = compile(st, uri, JStr(json.get_nt_p(e, "text")))
          ret_state
        })

      //list.each(changes, fn(e) { log("textDocument/didChanged: ~p", [e]) })

      //log("textDocument/didChanged: ~p", [jobj])

      #(None, new_state)
    }

    "textDocument/didClose" -> {
      #(None, state)
    }

    //let text = json.getp(jobj, ["params", "textDocument", "text"])
    //compile(state, uri, text)
    "textDocument/documentSymbol" -> {
      let uri = json.getp(jobj, ["params", "textDocument", "uri"])
      provide_symbol_information(state, id, uri)
    }

    _ -> {
      log("unknown message: ~p", [method])
      #(None, state)
    }
  }

  Reply(resp, new_state)
}

/// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#serverCapabilities 
pub fn handle_cast(msg: String, state: State) -> Response {
  Noreply(state)
}
