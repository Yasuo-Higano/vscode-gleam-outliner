import otp/erlang
import otp/io
import gleam
import lib/untyped.{untyped}
import lib/log.{log}
import gleam/list
import gleam/string
import lib/pprint.{format}
import gleam/option
//import gleam/json.{array, int, null, object, string}
import gleam/dynamic

pub type JPair {
  JPair(key: String, value: Json)
}

pub type Json {
  JObject(gleam.List(JPair))
  JStr(String)
  JInt(gleam.Int)
  JFloat(gleam.Float)
  JList(gleam.List(Json))
  JTrue
  JFalse
  JNull
}

@external(erlang, "ffi_erlang_json", "get_nt_p")
pub fn get_nt_p(j: Json, s: String) -> String

@external(erlang, "ffi_erlang_str", "to_gleam_str")
pub fn to_gleam_str(a: a) -> String

pub fn encode(jobj: Json) {
  to_gleam_str(enc_(jobj))
}

fn enc_pair(pair: JPair) {
  let JPair(k, v) = pair
  format("\"~s\":~s", [k, enc_(v)])
}

fn enc_obj(elms: List(JPair)) {
  let [_, ..rest]: List(String) =
    list.map(elms, fn(e) { [",", enc_pair(e)] })
    |> list.flatten
  let s = string.concat(rest)
  format("{~s}", [s])
}

fn enc_(jobj: Json) {
  case jobj {
    JObject([]) -> format("{}", [])
    JObject(objs) -> enc_obj(objs)

    JStr(s) -> format("\"~ts\"", [s])
    JInt(i) -> format("~p", [i])
    JFloat(f) -> format("~p", [f])
    JList([]) -> format("[]", [])
    JList(l) -> {
      let [_, ..rest] =
        list.map(l, fn(e) { [",", enc_(e)] })
        |> list.flatten
      let s = string.concat(rest)
      format("[~s]", [s])
    }
    _ -> format("null", [])
  }
}

@external(erlang, "erlang_ffi", "decode_json")
fn decode_json(s: String) -> Json

pub fn decode(src: String) -> Json {
  decode_json(src)
}

pub fn getp(json: Json, keyp: List(String)) -> Json {
  //log("json: ~p [[ ~p ]]", [untyped(json), untyped(keyp)])
  case keyp {
    [] -> JNull
    [k] -> get(json, k)
    [k, ..rest] -> {
      let obj = get(json, k)
      getp(obj, rest)
    }
  }
}

pub fn get(json: Json, key: String) -> Json {
  case json {
    JObject(objs) -> {
      let found =
        list.find(objs, fn(e) {
          let JPair(k, v) = e
          k == key
        })
      case found {
        Ok(JPair(_, val)) -> val
        _ -> JNull
      }
    }
    _ -> JNull
  }
}

pub fn string_of(json: Json) -> String {
  let assert JStr(str) = json
  str
}

pub fn int_of(json: Json) -> Int {
  let assert JInt(val) = json
  val
}

pub fn test() {
  let j =
    JObject([
      JPair("name", JStr("yasup")),
      JPair("age", JInt(14)),
      JPair("hobbies", JList([JStr("hack"), JStr("movie")])),
    ])
  log("~p", [j])
  let enc = encode(j)
  log("json: ~s", [enc])

  let src =
    "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":6699,\"clientInfo\":{\"name\":\"Visual Studio Code\",\"version\":\"1.64.2\"},\"locale\":\"ja\",\"rootPath\":\"/Users/yasuo/Workbench/PrjGleam/gleam/test\",\"rootUri\":\"file:///Users/yasuo/Workbench/PrjGleam/gleam/test\",\"capabilities\":{\"workspace\":{\"applyEdit\":true,\"workspaceEdit\":{\"documentChanges\":true,\"resourceOperations\":[\"create\",\"rename\",\"delete\"],\"failureHandling\":\"textOnlyTransactional\",\"normalizesLineEndings\":true,\"changeAnnotationSupport\":{\"groupsOnLabel\":true}},\"didChangeConfiguration\":{\"dynamicRegistration\":true},\"didChangeWatchedFiles\":{\"dynamicRegistration\":true},\"symbol\":{\"dynamicRegistration\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]},\"tagSupport\":{\"valueSet\":[1]}},\"codeLens\":{\"refreshSupport\":true},\"executeCommand\":{\"dynamicRegistration\":true},\"configuration\":true,\"workspaceFolders\":true,\"semanticTokens\":{\"refreshSupport\":true},\"fileOperations\":{\"dynamicRegistration\":true,\"didCreate\":true,\"didRename\":true,\"didDelete\":true,\"willCreate\":true,\"willRename\":true,\"willDelete\":true}},\"textDocument\":{\"publishDiagnostics\":{\"relatedInformation\":true,\"versionSupport\":false,\"tagSupport\":{\"valueSet\":[1,2]},\"codeDescriptionSupport\":true,\"dataSupport\":true},\"synchronization\":{\"dynamicRegistration\":true,\"willSave\":true,\"willSaveWaitUntil\":true,\"didSave\":true},\"completion\":{\"dynamicRegistration\":true,\"contextSupport\":true,\"completionItem\":{\"snippetSupport\":true,\"commitCharactersSupport\":true,\"documentationFormat\":[\"markdown\",\"plaintext\"],\"deprecatedSupport\":true,\"preselectSupport\":true,\"tagSupport\":{\"valueSet\":[1]},\"insertReplaceSupport\":true,\"resolveSupport\":{\"properties\":[\"documentation\",\"detail\",\"additionalTextEdits\"]},\"insertTextModeSupport\":{\"valueSet\":[1,2]}},\"completionItemKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]}},\"hover\":{\"dynamicRegistration\":true,\"contentFormat\":[\"markdown\",\"plaintext\"]},\"signatureHelp\":{\"dynamicRegistration\":true,\"signatureInformation\":{\"documentationFormat\":[\"markdown\",\"plaintext\"],\"parameterInformation\":{\"labelOffsetSupport\":true},\"activeParameterSupport\":true},\"contextSupport\":true},\"definition\":{\"dynamicRegistration\":true,\"linkSupport\":true},\"references\":{\"dynamicRegistration\":true},\"documentHighlight\":{\"dynamicRegistration\":true},\"documentSymbol\":{\"dynamicRegistration\":true,\"symbolKind\":{\"valueSet\":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]},\"hierarchicalDocumentSymbolSupport\":true,\"tagSupport\":{\"valueSet\":[1]},\"labelSupport\":true},\"codeAction\":{\"dynamicRegistration\":true,\"isPreferredSupport\":true,\"disabledSupport\":true,\"dataSupport\":true,\"resolveSupport\":{\"properties\":[\"edit\"]},\"codeActionLiteralSupport\":{\"codeActionKind\":{\"valueSet\":[\"\",\"quickfix\",\"refactor\",\"refactor.extract\",\"refactor.inline\",\"refactor.rewrite\",\"source\",\"source.organizeImports\"]}},\"honorsChangeAnnotations\":false},\"codeLens\":{\"dynamicRegistration\":true},\"formatting\":{\"dynamicRegistration\":true},\"rangeFormatting\":{\"dynamicRegistration\":true},\"onTypeFormatting\":{\"dynamicRegistration\":true},\"rename\":{\"dynamicRegistration\":true,\"prepareSupport\":true,\"prepareSupportDefaultBehavior\":1,\"honorsChangeAnnotations\":true},\"documentLink\":{\"dynamicRegistration\":true,\"tooltipSupport\":true},\"typeDefinition\":{\"dynamicRegistration\":true,\"linkSupport\":true},\"implementation\":{\"dynamicRegistration\":true,\"linkSupport\":true},\"colorProvider\":{\"dynamicRegistration\":true},\"foldingRange\":{\"dynamicRegistration\":true,\"rangeLimit\":5000,\"lineFoldingOnly\":true},\"declaration\":{\"dynamicRegistration\":true,\"linkSupport\":true},\"selectionRange\":{\"dynamicRegistration\":true},\"callHierarchy\":{\"dynamicRegistration\":true},\"semanticTokens\":{\"dynamicRegistration\":true,\"tokenTypes\":[\"namespace\",\"type\",\"class\",\"enum\",\"interface\",\"struct\",\"typeParameter\",\"parameter\",\"variable\",\"property\",\"enumMember\",\"event\",\"function\",\"method\",\"macro\",\"keyword\",\"modifier\",\"comment\",\"string\",\"number\",\"regexp\",\"operator\"],\"tokenModifiers\":[\"declaration\",\"definition\",\"readonly\",\"static\",\"deprecated\",\"abstract\",\"async\",\"modification\",\"documentation\",\"defaultLibrary\"],\"formats\":[\"relative\"],\"requests\":{\"range\":true,\"full\":{\"delta\":true}},\"multilineTokenSupport\":false,\"overlappingTokenSupport\":false},\"linkedEditingRange\":{\"dynamicRegistration\":true}},\"window\":{\"showMessage\":{\"messageActionItem\":{\"additionalPropertiesSupport\":true}},\"showDocument\":{\"support\":true},\"workDoneProgress\":true},\"general\":{\"regularExpressions\":{\"engine\":\"ECMAScript\",\"version\":\"ES2020\"},\"markdown\":{\"parser\":\"marked\",\"version\":\"1.1.0\"}}},\"trace\":\"off\",\"workspaceFolders\":[{\"uri\":\"file:///Users/yasuo/Workbench/PrjGleam/gleam/test\",\"name\":\"test\"}]}}"
  //let jobj = decode(enc)
  let jobj = decode(src)
  log("jobj = ~p", [jobj])
  log("method = ~p", [get(jobj, "method")])
  log("method = ~p", [string_of(get(jobj, "method"))])
  let id = int_of(get(jobj, "id"))
  log("id = ~p", [id])
}
