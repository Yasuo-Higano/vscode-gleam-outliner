import otp/erlang.{Unit}
import gleam
import lib/log.{log}
import gleam/io
import gleam/list
import gleam/string
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
//import gleam/json.{array, int, null, object, string}
import gleam/dynamic
import sola/exception
import sola/json.{
  JFalse, JFloat, JInt, JList, JNull, JObject, JPair, JStr, JTrue, Json,
}
import gleam_lsp/interpreter/token
import gleam_lsp/interpreter/lexer
import gleam_lsp/interpreter/itype.{SrcInf, Token2}
import gleam_lsp/interpreter/fuzzy_doc_symbolizer.{
  Constructor, DefConst, DefExternalFun, DefExternalType, DefType, DefTypeAlias,
  Defun, DocSymbol, DocSymbol2, ParentWithChildren, Range,
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

external fn init_stdio() -> Unit =
  "erlang_ffi" "init_stdio"

external fn io_write(device: atom, str: String) -> Unit =
  "io" "write"

external fn file_read_line(device: atom) -> String =
  "file" "read_line"

pub external fn format(fmt: String, param: a) -> List(Int) =
  "io_lib" "format"

pub external fn start_(a, b, c) -> Result(Pid, err) =
  "gen_server" "start_link"

pub external fn string_to_atom(str: String) -> Result(Atom, err) =
  "erlang" "binary_to_atom"

pub external fn list_to_binary(str: List(Int)) -> String =
  "erlang" "list_to_binary"

pub external fn call(pid: Pid, msg: dyn, timeout: Int) -> Option(String) =
  "gen_server" "call"

pub external fn cast(pid: Pid, msg: dyn) -> String =
  "gen_server" "cast"

pub external fn tuple_to_list(in: a) -> b =
  "erlang" "tuple_to_list"

pub fn start_gen_server() -> Result(Pid, err) {
  let global_atom = string_to_atom("global")
  let module_atom = string_to_atom("gleam_lsp@languageserver@server")
  //start_(#(global_atom, module_atom), [], [])
  start_(module_atom, [], [])
}

pub fn init(args: dyn) -> Result(State, a) {
  Ok(new_state())
}

//{ "jsonrpc": "2.0", "id": ID, "result": RESULT }
fn make_response(opt_id: Option(Json), msg: List(JPair)) {
  let l1 = [JPair("jsonrpc", JStr("2.0"))]
  let l2 =
    list.append(
      l1,
      case opt_id {
        Some(id) -> [JPair("id", id)]
        None -> []
      },
    )

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
    JPair(
      "selectionRange",
      range(Pos(r.sl, r.sc), Pos(r.sl, r.sc + symbol_len)),
    ),
    JPair("range", range(Pos(r.sl, r.sc), Pos(r.el, r.ec))),
  ]
  let lobj2 =
    list.append(
      lobj,
      case children {
        JList(_) -> [JPair("children", children)]
        _ -> []
      },
    )
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
  log("uri: ~p", [juri])

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
  log("uri: ~p", [j_uri])
  //log("text: ~p", [j_text])
  let assert JStr(uri) = j_uri
  let assert JStr(src) = j_text

  case
    exception.catch_ex(fn() {
      //log("**** LEXER", [])
      let tokens = lexer.parse(src)
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
      Some(make_response(
        id,
        [
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
        ],
      )),
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
      //list.each(changes, fn(e) { log("textDocument/didChanged: ~p", [e]) })
      #(None, state)
    }

    "textDocument/documentSymbol" -> {
      let uri = json.getp(jobj, ["params", "textDocument", "uri"])
      provide_symbol_information(state, id, uri)
    }

    _ -> #(None, state)
  }

  //log("reply ~p", [resp])
  //io.println(format("call:~p | ~p", tuple_to_list(#(msg, state))))
  //let jobj = json.decode_to_dynamic(msg)
  Reply(resp, new_state)
}

/// https://microsoft.github.io/language-server-protocol/specifications/specification-3-17/#serverCapabilities 
pub fn handle_cast(msg: String, state: State) -> Response {
  //io.println(format("cast:~p | ~p", tuple_to_list(#(msg, state))))
  Noreply(state)
}
