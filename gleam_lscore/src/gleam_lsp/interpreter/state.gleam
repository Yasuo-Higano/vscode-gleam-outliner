import otp/io
import otp/erlang.{Atom, Unit, atom_of, float, trunc}
import otp/global
import lib/object
import gleam/order
import lib/pprint.{format}
import gleam_lsp/interpreter/lexer
import gleam_lsp/interpreter/token
import lib/log.{log}
import gleam/string
import gleam/list
import gleam/map
import gleam/set
import gleam/option.{None, Option, Some}
import gleam/int
import gleam/float
import lib/untyped.{Untyped, untyped}
import otp/file as erl_file
import sola/exception
import sola/tuple
import haxe/sys/io/file
import gleam_lsp/interpreter/fuzzy_doc_symbolizer
import gleam_lsp/interpreter/ast
import gleam_lsp/interpreter/itype.{
  AtomVal, BinOp, Block, Boolean, Case, CaseClause, Closure, Comment,
  CompileError, CompilerState, Cond, CondClause, CustomType, DefArg, DefConst,
  DefCustomType, DefExternalFun, DefExternalType, DefParam, DefTypeAlias, Defun,
  DiscardName, Env, ErrorMsg, Expected, Expr2, ExternalFun, FloatVal, Fun,
  FunRef, Funcall, Import, ImportAs, ImportUnqualified, IntVal, Let, ListEx,
  ListExWithTail, ListTypeUnmatch, MemberAccess, Module, ModulePath, ModuleRef,
  NOP, Name, NativeFun, NativeFunReturn, NativeSigil, Object, Quote,
  RecordConstructor, RecordConstructorArg, RecordData, RecordUpdate,
  RecordUpdateArg, RecordUpdateSpread, Sequence, Set, SigilExpr, SrcInf,
  StringVal, T, TCustomType, TFloat, TInt, TList, TObject, TString, TTuple,
  TUnit, TUnknown, Token2, Tuple, TypeAccess, TypeMismatchAB, TypeName, TypeRef,
  TypeVal, UnboundValue, UnitVal, UnqualifiedImport, UnqualifiedImportType,
  UnqualifiedImportVal, When,
}
import gleam_lsp/interpreter/color.{cyan, green, red, yellow}

pub fn eofinf() {
  SrcInf(0, 0, None)
}

pub fn nopexpr2() {
  Expr2(NOP, eofinf())
}

fn env_path_(env: Env, acc: List(String)) {
  case env.parent {
    None ->
      list.reverse(acc)
      |> string.concat
    Some(parent) -> env_path_(parent, ["/", env.name, ..acc])
  }
}

pub fn env_path(env: Env) {
  env_path_(env, [])
}

pub fn list_types(env: Env) -> Unit {
  log("~s types =====================================", [env.name])
  map.fold(
    env.types,
    [],
    fn(acc, k, v) {
      log("~p: ~s", [untyped(k), untyped(itype.pcustomtype(v))])
      []
    },
  )

  case env.parent {
    None -> None
    Some(parent) -> list_types(parent)
  }
}

pub fn list_vars_ex(env: Env) -> Unit {
  log("\n~s vars =====================================", [env.name])
  map.fold(
    env.m,
    [],
    fn(acc, k, v) {
      let Expr2(_, SrcInf(line, col, _)) = v
      log("~s: ~s(~p,~p)", #(k, itype.pformat2(v), line, col))
      []
    },
  )

  case env.parent {
    None -> None
    Some(parent) -> list_vars_ex(parent)
  }
}

pub fn list_vars(env: Env) -> Unit {
  log("\n~s vars =====================================", [env.name])
  map.fold(
    env.m,
    [],
    fn(acc, k, v) {
      log("~s: ~s", [k, itype.pformat2(v)])
      []
    },
  )

  case env.parent {
    None -> None
    Some(parent) -> list_vars(parent)
  }
}

pub fn list_vars2(env: Env) {
  log("\n~s vars ##############################################", [env.name])
  list_vars2_(env)
  let assert Some(parent) = env.parent
  list_vars2_(parent)
  log("\n", [])
}

pub fn list_vars2_(env: Env) {
  log("\n~s =====================================", [env.name])
  list.each(
    map.keys(env.m),
    fn(k) {
      let assert Ok(v) = map.get(env.m, k)

      log("~s: ~s", [k, itype.pformat2(v)])
    },
  )
}

pub fn list_types2(env: Env) {
  log("\n~s types #############################################", [env.name])
  list_types2_(env)
  let assert Some(parent) = env.parent
  list_types2_(parent)
  log("\n", [])
}

pub fn list_types2_(env: Env) {
  log("~s =====================================", [env.name])
  map.fold(
    env.types,
    [],
    fn(acc, k, v) {
      log("~p: ~s", [untyped(k), untyped(itype.pcustomtype(v))])
      []
    },
  )
}

pub fn lookup_type(env: Env, name: Atom) {
  case map.get(env.types, name) {
    Ok(val) -> Some(val)
    _ ->
      case env.parent {
        None -> None
        Some(parent) -> lookup_type(parent, name)
      }
  }
}

pub fn lookup_var(env: Env, name: String) {
  case map.get(env.m, name) {
    Ok(val) -> Some(val)
    _ ->
      case env.parent {
        None -> None
        Some(parent) -> lookup_var(parent, name)
      }
  }
}

pub fn bind_to_public(modenv: Env, a: Expr2, b: Expr2) {
  let assert Some(pubenv) = modenv.parent
  case a {
    Expr2(Name(t: t, name: name, renamed: _), _) ->
      Env(
        ..modenv,
        parent: Some(Env(..pubenv, m: map.insert(pubenv.m, name, b))),
      )
    _ -> modenv
  }
}

pub fn bind(env: Env, a: Expr2, b: Expr2) {
  case a {
    Expr2(Name(t: t, name: name, renamed: _), _) -> {
      log("#### bind OK: ~s = ~p", [untyped(name), untyped(b)])
      Env(..env, m: map.insert(env.m, name, b))
    }
    _ -> {
      log("#### bind failed!!! ~p = ~p", [a, b])
      env
    }
  }
}

pub fn bind_to_public_(modenv: Env, name: String, b: Expr2) {
  let assert Some(pubenv) = modenv.parent
  Env(..modenv, parent: Some(Env(..pubenv, m: map.insert(pubenv.m, name, b))))
}

pub fn bind_(env: Env, name: String, b: Expr2) {
  Env(..env, m: map.insert(env.m, name, b))
}

pub fn bind_type_to_public_(modenv: Env, name: Atom, b: RecordConstructor) {
  let assert Some(pubenv) = modenv.parent
  Env(
    ..modenv,
    parent: Some(Env(..pubenv, types: map.insert(pubenv.types, name, b))),
  )
}

pub fn bind_type_(env: Env, name: Atom, b: RecordConstructor) {
  Env(..env, types: map.insert(env.types, name, b))
}

pub fn typename_of(name: String) {
  erlang.atom_of(name)
}

pub fn global_env(cstate: CompilerState) {
  let global_vars =
    map.new()
    |> map.insert(
      "println",
      Expr2(
        ExternalFun(
          t: TString,
          module: "erlang_ffi",
          fun: "println",
          argtype: [TString],
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "debug_assert",
      Expr2(
        ExternalFun(
          t: TString,
          module: "erlang_ffi",
          fun: "debug_assert",
          argtype: [TString],
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "debug_assert_equal",
      Expr2(
        ExternalFun(
          t: TString,
          module: "erlang_ffi",
          fun: "debug_assert_equal",
          argtype: [TString],
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "list_types",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            list_types(env)
            NativeFunReturn(env, Expr2(UnitVal, eofinf()))
          },
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "list_vars",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            //list_vars(env)
            list_vars_ex(env)
            NativeFunReturn(env, Expr2(UnitVal, eofinf()))
          },
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "type_of",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            let assert Ok(e) = list.first(params)
            NativeFunReturn(env, Expr2(TypeVal(itype.get_type2(e)), eofinf()))
          },
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "pwd",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            let assert Ok(dir) = erl_file.get_cwd()
            NativeFunReturn(env, Expr2(StringVal(dir), eofinf()))
          },
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "files",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            let assert Ok(files) = erl_file.list_dir("./")
            NativeFunReturn(
              env,
              Expr2(
                ListEx(
                  t: TString,
                  elements: list.map(
                    files,
                    fn(f) { Expr2(StringVal(f), eofinf()) },
                  ),
                ),
                eofinf(),
              ),
            )
          },
        ),
        eofinf(),
      ),
    )
    |> map.insert(
      "object",
      Expr2(
        NativeFun(
          t: TList(TString),
          args: [],
          fun: fn(env: Env, params: List(Expr2)) {
            NativeFunReturn(
              env,
              Expr2(Object(object.new_from_list([])), eofinf()),
            )
          },
        ),
        eofinf(),
      ),
    )

  let globalenv =
    Env(
      name: "global",
      m: global_vars,
      rnames: map.new(),
      types: map.new(),
      parent: None,
      state: cstate,
    )
}

// -----------------------------------------------------------------------------------------------
pub fn module_put(path: String, module: Module) {
  global.put(erlang.atom_of(path), module)
  module
}

pub fn module_get(path: String) -> Module {
  global.get2(erlang.atom_of(path), empty_module())
}

pub fn module_get_type(path: String, constructor: String) {
  log("## module_get_type ~s ~s", [path, constructor])
  let mod = module_get(path)
  log("## mod = ~p", [mod])
  let assert rcon = case lookup_type(mod.env, erlang.atom_of(constructor)) {
    Some(x) -> x
    None ->
      RecordConstructor(
        format("#module_get_failed: ~p", [constructor]),
        [],
        map.new(),
      )
  }
}

pub fn module_exists(path: String) -> Bool {
  let atom = erlang.atom_of(path)
  log("atom = ~p", [atom])
  global.exists(atom)
}

pub fn get_arity(env: Env, name: String) -> Int {
  case lookup_var(env, name) {
    Some(Expr2(ExternalFun(t, module, fun, argtype), _)) -> list.length(argtype)
    Some(Expr2(Defun(t, public, name, args, body), _)) -> list.length(args)
    a ->
      //log("@@@@@@@@@@@get_arity: ~p", [a])
      0
  }
}

pub fn make_module_filepath(dirs: List(String), name: String) {
  let dirpath =
    list.map(dirs, fn(d) { ["/", d] })
    |> list.flatten
    |> string.concat
  let fpath = format("~s~s/~s.gleam", ["scripts", dirpath, name])
  fpath
}

pub fn module_path(module: List(String), package: String) -> ModulePath {
  ModulePath(module, package)
}

pub fn erl_module_path(module_path: ModulePath) -> String {
  let ModulePath(module, package) = module_path
  case module {
    [] -> package
    _ -> {
      let [_, ..r] =
        list.map(module, fn(e) { ["@", e] })
        |> list.flatten
      let path = string.concat(r)
      format("~s@~s", [path, package])
    }
  }
}

pub fn local_env(name: String, env: Env) {
  Env(
    name: name,
    m: map.new(),
    rnames: map.new(),
    types: map.new(),
    parent: Some(env),
    state: env.state,
  )
}

fn init_compiler_state() {
  let defs: List(String) = []
  let prgs: List(String) = []
  let defset: set.Set(String) = set.new()
  let obj =
    object.new()
    |> object.set("indent", 0)
    |> object.set("defs", defs)
    |> object.set("prgs", prgs)
    |> object.set("defset", defset)
  CompilerState(obj)
}

fn empty_module() {
  let state = init_compiler_state()
  let env =
    Env(
      name: "",
      m: map.new(),
      rnames: map.new(),
      types: map.new(),
      parent: None,
      state: state,
    )
  Module("", env, [])
}

pub fn new_module(name: String) -> Module {
  let cstate = init_compiler_state()
  let globalenv = global_env(cstate)
  let publicenv =
    Env(
      name: format("~s:public", [name]),
      m: map.new(),
      rnames: map.new(),
      types: map.new(),
      parent: Some(globalenv),
      state: cstate,
    )
  let moduleenv =
    Env(
      name: name,
      m: map.new(),
      rnames: map.new(),
      types: map.new(),
      parent: Some(publicenv),
      state: cstate,
    )
  Module(env: moduleenv, name: name, src: [])
}

pub fn make_ast(tokens: List(Token2), acc) {
  case tokens {
    [] -> list.reverse(acc)
    _ -> {
      let #(code, rest) = ast.parse_ast(tokens)
      make_ast(rest, [code, ..acc])
    }
  }
}
