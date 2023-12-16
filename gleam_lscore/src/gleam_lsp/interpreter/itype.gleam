import otp/io
import otp/erlang.{type Atom, type Term}
import lib/pprint.{format}
import gleam_lsp/interpreter/token.{type Token}
import lib/log.{log}
import gleam/string
import gleam/list
import gleam/map.{type Map}
import gleam/option.{type Option, None, Some}
import gleam/int
import gleam/float
import gleam
import lib/untyped.{untyped}
import sola/exception
import sola/tuple
import lib/object.{type Object}
import gleam/dynamic.{type Dynamic}

pub type CompilerState {
  CompilerState(ref: Object)
}

pub type Env {
  Env(
    name: String,
    m: Map(String, Expr2),
    rnames: Map(String, String),
    types: Map(Atom, RecordConstructor),
    parent: Option(Env),
    state: CompilerState,
  )
}

pub type ModulePath {
  ModulePath(List(String), String)
}

pub type Module {
  Module(name: String, env: Env, src: List(Expr2))
}

pub type Token2 {
  Token2(token: Token, pos: SrcInf)
}

pub type Inf {
  Inf(compiled: Expr)
}

pub type SrcInf {
  SrcInf(line: Int, col: Int, inf: Option(Inf))
}

pub type CommentType {
  ModuleComment
  DocComment
  NormalComment
}

pub type CommentStyle {
  LineComment
  BlockComment
}

pub type T {
  TUnknown
  TDynamic
  TUnit
  TType(T)
  TInt
  TFloat
  TString
  TChar
  TBool
  TAtom
  TList(T)
  TTuple(List(T))
  TSTTuple1(T)
  TSTTuple2(T, T)
  TSTTuple3(T, T, T)
  TSTTuple4(T, T, T, T)
  TSTTuple5(T, T, T, T, T)
  TCustomType(String, List(T))
  TProxyType(String, List(T))
  TComment(CommentType, CommentStyle)
  TSigil(T)
  TObject
  TTypeSig(String)
}

// Error(inf: SrcInf)
// CompileError : Error
pub type CompileError {
  ParseError(rest: List(Token2), error: CompileError)
  Expected(inf: SrcInf, msg: String, a: String, b: String)
  UnboundValue(inf: SrcInf, name: String)
  ListTypeUnmatch(inf: SrcInf, a: T, b: T)
  ErrorType(inf: SrcInf, a: T)
  ErrorMsg(inf: SrcInf, msg: String, msg2: Dynamic)
  TypeMismatchAB(inf: SrcInf, a: T, b: T)
  UnexpectedEOF
}

//(inf: SrcInf)
//pub type Arg {
//    UntypedArg(name:String)
//    TypedArg(name:String, typename:String)
//    UnnamedTypedArg(typename:String)
//}
// type Type {
//   RecordConstructor( RecordConstructorArg )
//   RecordConstructor( RecordConstructorArg )
// }
pub type RecordConstructor {
  RecordConstructor(
    //location: SrcInf,
    name: String,
    arguments: List(RecordConstructorArg),
    label_to_index: Map(String, Int),
  )
}

//documentation: Option<String>,
pub type RecordConstructorArg {
  RecordConstructorArg(t: T, label: Option(String))
}

//ast: TypeAst,
//location: SrcInf,
//doc: Option<String>,
// Record(..RecordUpdateSpread, RecordUpdateArg, RecordUpdateArg)
pub type RecordUpdateSpread {
  RecordUpdateSpread(base: Expr2)
}

pub type RecordUpdateArg {
  RecordUpdateArg(label: Option(String), value: Expr2)
}

pub type DefArg {
  DefArg(t: T, name: String, renamed: String, default: Option(Expr2))
}

pub type DefParam {
  DefParam(t: T, name: Option(String), expr: Expr2)
}

pub type CondClause {
  CondClause(t: T, expr: Expr2, then: Expr2)
}

// case EXPR {
//      Clause pattern -> then
// }
pub type CaseClause {
  CaseClause(
    t: T,
    pattern: Expr2,
    //alternative_patterns: Vec<MultiPattern<PatternConstructor, Type>>,
    guard: Option(CondClause),
    then: Expr2,
  )
}

pub type UnqualifiedImport {
  UnqualifiedImportType(name: String, as_name: Option(String))
  UnqualifiedImportVal(name: String, as_name: Option(String))
}

//pub type TExpr {
//  Typed(T, Expr)
//  Untyped(Expr)
//}
pub type NativeFunReturn {
  NativeFunReturn(Env, Expr2)
}

pub type Expr2 {
  Expr2(expr: Expr, srcinf: SrcInf)
}

pub type Expr {
  NOP
  Typed(Expr2)
  UnitVal
  Quote(Expr2)
  //Arg(DefArg)
  //Char(Int)
  TokenEx(token.Token)
  TypeVal(t: T)
  IntVal(value: Int)
  FloatVal(value: Float)
  StringVal(value: String)
  AtomVal(Atom)
  Boolean(gleam.Bool)
  DiscardName(t: T, name: String)
  Name(t: T, name: String, renamed: Option(String))

  // var, const var
  TypeName(name: String)
  DefConst(t: T, public: Bool, name: Expr2, value: Expr2)
  // const name = Expr
  Set(t: T, pattern: Expr2, value: Expr2)
  // let pattern = value
  Let(t: T, pattern: Expr2, value: Expr2)
  // let pattern = value
  BinOp(t: T, name: token.Token, left: Expr2, right: Expr2)
  NativeFun(
    t: T,
    args: List(DefArg),
    fun: fn(Env, List(Expr2)) -> NativeFunReturn,
  )
  NativeSigil(t: T, args: List(DefArg), fun: fn(Env, Expr2) -> NativeFunReturn)
  //ExternalFun(t: T, args: List(DefArg), fun:fn(Env,List(Expr2))->Expr2)
  Defun(t: T, public: Bool, name: String, args: List(DefArg), body: Expr2)
  Fun(t: T, name: Option(String), args: List(DefArg), body: Expr2)
  Closure(t: T, args: List(DefArg), body: Expr2, env: Env)
  Funcall(t: T, fun: Expr2, params: List(DefParam))
  Block(t: T, expressions: List(Expr2))
  Sequence(t: T, expressions: List(Expr2))
  ListEx(t: T, elements: List(Expr2))
  ListExWithTail(t: T, elements: List(Expr2), tail: token.Token)
  Tuple(elements: List(Expr2))
  SigilExpr(sigil: String, expr: Expr2)
  MemberAccess(t: T, expr: Expr2, member: String)
  TypeAccess(t: T, expr: Expr2, member: String)

  Case(t: T, subjects: Expr2, clauses: List(CaseClause))
  Cond(t: T, clauses: List(CondClause))
  When(subjects: Expr2, body: List(Expr2))

  RecordUpdate(
    constructor: Expr2,
    // Expr2,
    spread: Option(RecordUpdateSpread),
    arguments: List(RecordUpdateArg),
  )

  // 評価済みのExpr2
  RecordData(erlang.Tuple)

  Comment(comment: token.Token)

  // import module/module/package
  Import(module: List(String), package: String)

  // import module/module/package as as_name
  ImportAs(module: List(String), package: String, as_name: String)
  // import module/module/package.{unqualified,unqualified}
  ImportUnqualified(
    module: List(String),
    package: String,
    unqualified: List(UnqualifiedImport),
  )

  ModuleRef(ModulePath)
  FunRef(String)
  TypeRef(String)

  //    /// pub const name = value
  //    ModuleConstant (
  //        public: Bool,
  //        name: String,
  //        value: Expr2,
  //    )
  ExternalFun(t: T, module: String, fun: String, argtype: List(T))

  /// pub external fn name(Type,Type) = "module" "fun"
  DefExternalFun(
    t: T,
    public: Bool,
    name: String,
    module: String,
    fun: String,
    argtype: List(T),
  )

  /// pub external type Name(Type,Type) = TypeName(Type,Type) 
  DefExternalType(public: Bool, name: String, arguments: List(DefArg))

  /// pub type TypeName = Type(Arg,Arg)
  DefTypeAlias(
    t: T,
    public: Bool,
    alias: String,
    parameters: Option(List(String)),
  )

  //TypeAlias(t: T, alias: String, parameters: Option(List(String)))
  /// pub type Name(RecordConstructorArg) {
  ///     RecordConstructor
  ///     RecordConstructor
  /// }
  DefCustomType(
    name: String,
    parameters: Option(List(RecordConstructorArg)),
    public: Bool,
    constructors: List(RecordConstructor),
  )

  CustomType(
    name: String,
    parameters: Option(List(RecordConstructorArg)),
    constructors: List(RecordConstructor),
  )

  Object(object.Object)
}

pub fn token_to_str(t: token.Token) {
  case t {
    token.Plus -> "+"
    token.Minus -> "-"
    token.Star -> "*"
    token.Slash -> "/"
    token.ColonEqual -> ":="
    token.Equal -> "="
    token.Less -> "<"
    token.LessEqual -> "=<"
    token.Greater -> ">"
    token.GreaterEqual -> ">="
    token.Name(name) -> name
    token.UpName(name) -> name
    _ -> format("~p", [t])
  }
}

pub fn pargs(args: List(DefArg)) {
  case args {
    [] -> ""
    _ -> {
      let [_, ..strlst] =
        list.map(args, fn(arg) {
          case arg {
            DefArg(
              t: t,
              name: name,
              renamed: renamed,
              default: opt_default_value,
            ) -> {
              let typstr = case t {
                TUnknown -> ""
                _ -> format(":~s", [type_name(t)])
              }
              let valstr = case opt_default_value {
                Some(val) -> format(" = ~s", [pformat2(val)])
                _ -> ""
              }
              [", ", format("~s(~s)~s~s", [name, renamed, typstr, valstr])]
            }
          }
        })
        |> list.flatten
      string.concat(strlst)
    }
  }
}

pub fn pcustomtype(ctype: RecordConstructor) {
  pconstructor(ctype)
}

pub fn pparams(params: List(DefParam)) {
  format("~p", [params])
}

pub fn precargs(params: List(RecordConstructorArg)) {
  let [_, ..lst] =
    list.map(params, fn(e) { [", ", precarg(e)] })
    |> list.flatten

  string.concat(lst)
}

// label:Type
pub fn precarg(arg: RecordConstructorArg) {
  let label = case arg.label {
    Some(label) -> format("~s:~s", [label, type_name(arg.t)])
    None -> type_name(arg.t)
  }
}

// Record(name:Type, name:Type)
pub fn pconstructor(c: RecordConstructor) {
  case c.arguments {
    [] -> c.name
    _ -> {
      let [_, ..lstargs] =
        list.flatten(list.map(c.arguments, fn(e) { [", ", precarg(e)] }))
      let strargs = string.concat(lstargs)
      format("\t~s(~s)", [c.name, strargs])
    }
  }
}

// {
//   Record(name:Type, name:Type)
//   Record(name:Type, name:Type)
//}
pub fn pconstructors(params: List(RecordConstructor)) {
  list.map(params, fn(e) { pconstructor(e) })
  |> string.concat
}

pub fn pformat2(code2: Expr2) {
  let Expr2(code, inf) = code2
  pformat(code)
}

pub fn pformat(code: Expr) {
  case code {
    NOP -> "[]"
    UnitVal -> "Unit"
    TypeVal(t: t) -> type_name(t)
    IntVal(val) -> format("~p", [val])
    FloatVal(val) -> format("~p", [val])
    StringVal(val) -> format("\"~s\"", [val])
    AtomVal(val) -> format("'\"~s\"", [val])
    Name(t: t, name: name, renamed: opt_renamed) ->
      case t {
        TUnknown -> format("~s", [name])
        _ -> format("~s:~s", [name, type_name(t)])
      }
    ListEx(t: t, elements: elements) ->
      case elements {
        [] -> "[]"
        _ -> {
          let [_, ..rest] =
            list.fold(elements, [], fn(acc, e) { [pformat2(e), ", ", ..acc] })
            |> list.reverse
          format("[~s]", [string.concat(rest)])
        }
      }
    Tuple(elements: elements) ->
      case elements {
        [] -> "#()"
        _ -> {
          let [_, ..rest] =
            list.fold(elements, [], fn(acc, e) { [pformat2(e), ", ", ..acc] })
            |> list.reverse
          format("#( ~s)", [string.concat(rest)])
        }
      }

    NativeFun(t: t, args: args, fun: _) ->
      format("native_function(~s) -> ~s", [pargs(args), type_name(t)])
    ExternalFun(t: t, module: module, fun: fun, argtype: argtype) ->
      format("external_function (~p) -> ~s", [
        untyped(argtype),
        untyped(type_name(t)),
      ])
    DefExternalFun(
      t: t,
      public: public,
      name: name,
      module: module,
      fun: fun,
      argtype: argtype,
    ) ->
      format("def_external_function ~s(~p) -> ~s", [
        untyped(name),
        untyped(argtype),
        untyped(type_name(t)),
      ])
    Funcall(t: t, fun: fun, params: params) ->
      //format("~s(~s) -> ~s",[pformat(fun), pparams(params), type_name(t)])
      format("~s(~s) -> ~s", ["fun", pparams(params), type_name(t)])

    MemberAccess(t: t, expr: expr, member: member) ->
      format("~s.~s", [pformat2(expr), member])

    Comment(token.CommentModule(comment)) -> format("////~s", [comment])
    Comment(token.CommentDoc(comment)) -> format("///~s", [comment])
    Comment(token.CommentNormal(comment)) -> format("//~s", [comment])
    Comment(token.BlockCommentModule(comment)) -> format("/***~s*/", [comment])
    Comment(token.BlockCommentDoc(comment)) -> format("/**~s*/", [comment])
    Comment(token.BlockCommentNormal(comment)) -> format("/*~s*/", [comment])

    Import(module: module, package: package) ->
      format("import ~p ~s", [untyped(module), untyped(package)])

    ImportAs(module: module, package: package, as_name: as_name) ->
      format("import ~p ~s as ~s", [
        untyped(module),
        untyped(package),
        untyped(as_name),
      ])

    ImportUnqualified(
      module: module,
      package: package,
      unqualified: unqualified,
    ) ->
      format("import ~p ~s . { ~p }", [
        untyped(module),
        untyped(package),
        untyped(unqualified),
      ])

    Defun(t, public, name, args, body) ->
      format("defun ~s(~p) -> ~s", [
        untyped(name),
        untyped(args),
        untyped(type_name(t)),
      ])
    Fun(t: t, name: optname, args: args, body: expr) ->
      case optname {
        Some(name) -> format("fn ~s(~p)", [untyped(name), untyped(args)])
        None -> format("fn (~p)", [args])
      }

    Closure(t: t, args: args, body: expr, env: env) ->
      //format(
      //  "closure (~s) -> ~s\n~p",
      //  [untyped(pargs(args)), untyped(type_name(t)), untyped(expr)],
      //)
      format("closure (~s) -> ~s", [untyped(pargs(args)), untyped(type_name(t))])

    Object(_) -> format("Object", [])

    CustomType(name: name, parameters: opt_params, constructors: constructors) -> {
      let strbody = case constructors {
        [] -> ""
        _ -> format(" {\n~s\n}", [pconstructors(constructors)])
      }
      case opt_params {
        Some(params) -> format("~s(~s)~s", [name, precargs(params), strbody])
        None -> format("~s~s", [name, strbody])
      }
    }

    Boolean(f) ->
      case f {
        True -> "True"
        False -> "False"
      }

    RecordData(rec) ->
      case rec {
        //        #(name) -> name
        //        #(name, p1) -> format("~s(~s)", [name, pprint(p1)])
        //        #(name, p1, p2) -> format("~s(~s, ~s)", [name, pprint(p1), pprint(p2)])
        //        #(name, p1, p2, p3) ->
        //          format("~s(~s, ~s, ~s)", [name, pprint(p1), pprint(p2), pprint(p3)])
        //        #(name, p1, p2, p3, p4) ->
        //          format(
        //            "~s(~s, ~s, ~s, ~s)",
        //            [name, pprint(p1), pprint(p2), pprint(p3), pprint(p4)],
        //          )
        //        #(name, p1, p2, p3, p4, p5) ->
        //          format(
        //            "~s(~s, ~s, ~s, ~s, ~s)",
        //            [name, pprint(p1), pprint(p2), pprint(p3), pprint(p4), pprint(p5)],
        //          )
        _ -> format("~p", [rec])
      }

    _ -> format("??? ~p", [code])
  }
}

pub fn type_names(elements: List(T)) {
  let [_, ..rest] =
    list.map(elements, fn(e) { [", ", type_name(e)] })
    |> list.flatten
  string.concat(rest)
}

pub fn type_name(t: T) {
  case t {
    TComment(ModuleComment, LineComment) -> "Comment(Module,Line)"
    TComment(DocComment, LineComment) -> "Comment(Doc   ,Line)"
    TComment(NormalComment, LineComment) -> "Comment(Normal,Line)"
    TComment(ModuleComment, BlockComment) -> "Comment(Module,Block)"
    TComment(DocComment, BlockComment) -> "Comment(Doc   ,Block)"
    TComment(NormalComment, BlockComment) -> "Comment(Normal,Block)"
    TUnknown -> "Unknown"
    TUnit -> "[]"
    TType(t) -> format("Type(~s)", [type_name(t)])
    TInt -> "Int"
    TFloat -> "Float"
    TString -> "String"
    TAtom -> "Atom"
    TList(t) -> format("List(~s)", [type_name(t)])
    TTuple(elements) ->
      case elements {
        [] -> "#()"
        _ -> format("#(~s)", [type_names(elements)])
      }

    TObject -> "Object"
    TBool -> "Boolean"

    _ -> format("?????????? ~p", [t])
  }
}

pub fn ptype(code2: Expr2) {
  type_name(get_type2(code2))
}

pub fn pprint(code2: Expr2) {
  format("~s = ~s", [ptype(code2), pformat2(code2)])
}

pub fn perror(error: CompileError) {
  case error {
    UnboundValue(inf, name) -> format("Unbound value ~s", [name])
    ListTypeUnmatch(inf, a, b) ->
      format("List has different types ~s, ~s", [type_name(a), type_name(b)])
    TypeMismatchAB(inf, a, b) ->
      format("Type mismatch ~s, ~s", [type_name(a), type_name(b)])
    ErrorMsg(inf, msg, msg2) -> format("~s~p", [untyped(msg), untyped(msg2)])
    _ -> format("~p", [error])
  }
}

pub fn get_type2(code2: Expr2) {
  let Expr2(code, _) = code2
  get_type(code)
}

pub fn get_type(code: Expr) {
  case code {
    NOP -> TUnknown
    SigilExpr(_, expr) -> TSigil(get_type2(expr))
    UnitVal -> TUnit
    TypeVal(t) -> TType(t)
    IntVal(_) -> TInt
    FloatVal(_) -> TFloat
    StringVal(_) -> TString
    AtomVal(_) -> TAtom
    Name(t, _, _) -> t
    TypeName(typename) -> type_from_typename(typename)
    ListEx(t, _) -> TList(t)
    //Tuple(elements) -> TTuple( erlang.tuple_size(elements) )
    Tuple(elements) -> TTuple(list.map(elements, fn(e) { get_type2(e) }))
    NativeFun(t, _, _) -> t
    Funcall(t: t, fun: _, params: _) -> t
    Comment(comment) ->
      case comment {
        token.CommentModule(_) -> TComment(ModuleComment, LineComment)
        token.CommentDoc(_) -> TComment(DocComment, LineComment)
        token.CommentNormal(_) -> TComment(NormalComment, LineComment)
        token.BlockCommentModule(_) -> TComment(ModuleComment, BlockComment)
        token.BlockCommentDoc(_) -> TComment(DocComment, BlockComment)
        token.BlockCommentNormal(_) -> TComment(NormalComment, BlockComment)
        _ -> TUnknown
      }
    Import(_, _) -> TUnknown
    ImportAs(_, _, _) -> TUnknown
    ImportUnqualified(_, _, _) -> TUnknown

    ExternalFun(t: t, module: _, fun: _, argtype: _) -> t

    DefExternalFun(t: t, public: _, name: _, module: _, fun: _, argtype: _) ->
      TUnknown

    Object(_) -> TObject
    MemberAccess(t: t, expr: expr, member: member) -> t
    TypeAccess(t: t, expr: expr, member: member) -> t

    Boolean(f) -> TBool
    BinOp(t, _, _, _) -> t

    _ -> TUnknown
  }
}

pub fn type_from_typename(typename: String) -> T {
  case typename {
    "Int" -> TInt
    "Float" -> TFloat
    "String" -> TString
    "Dynamic" -> TDynamic
    _ -> TTypeSig(typename)
  }
}

pub fn get_record_type(e: Expr) -> Atom {
  let assert RecordData(dat) = e
  tuple.at(dat, 1)
}

@external(erlang, "erlang_ffi", "box")
pub fn box(val: a) -> Expr

@external(erlang, "erlang_ffi", "unbox")
pub fn unbox(val: Expr) -> Term

pub fn record_data_to_tuple(rec: erlang.Tuple) -> Expr {
  Tuple(
    elements: list.map(list.range(1, tuple.size(rec) + 1), fn(i) {
      tuple.at(rec, i)
    }),
  )
}

pub fn string_of(e: Expr) {
  let assert StringVal(str) = e
  str
}

pub fn int_of(e: Expr) {
  case e {
    StringVal(str) -> {
      let assert Ok(val) = int.parse(str)
      val
    }
    _ -> {
      let assert IntVal(val) = e
      val
    }
  }
}
