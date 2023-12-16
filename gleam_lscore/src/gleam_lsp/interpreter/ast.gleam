import otp/io
import gleam
import lib/pprint.{format}
import gleam_lsp/interpreter/lexer
import gleam_lsp/interpreter/token
import lib/log.{log}
import gleam/string
import gleam/dynamic
import gleam/list
import gleam/map
import gleam/option.{type Option, None, Some}
import gleam/int
import gleam/float
import lib/untyped.{untyped}
import sola/exception
import gleam_lsp/interpreter/itype.{
  type CaseClause, type CompileError, type CondClause, type DefArg,
  type DefParam, type Expr2, type RecordConstructor, type RecordConstructorArg,
  type SrcInf, type T, type Token2, type UnqualifiedImport, BinOp, Boolean,
  CaseClause, CondClause, DefArg, DefConst, DefCustomType, DefExternalFun,
  DefExternalType, DefParam, DefTypeAlias, Defun, Expected, Expr2, Fun, Import,
  ImportAs, ImportUnqualified, NOP, RecordConstructor, RecordConstructorArg,
  SrcInf, TCustomType, TList, TProxyType, TTuple, TUnknown, Token2, TypeAccess,
  UnqualifiedImportType, UnqualifiedImportVal,
}

fn eofinf() {
  SrcInf(0, 0, None)
}

fn nopexpr2() {
  Expr2(NOP, eofinf())
}

fn block(name: String, aux: b, f: fn() -> a) -> a {
  //log("<<<<Block ~s: ~p", [untyped(name), untyped(aux)])
  let r = f()
  //log("Block ~s>>>>", [name])
  r
}

fn srcinf_of(a: Token2) -> SrcInf {
  let Token2(_, inf) = a
  inf
}

fn raise_parse_error(rest: List(Token2), error: CompileError) {
  exception.throw_ex(itype.ParseError(rest, error))
}

fn expect(src: List(Token2), ex: List(token.Token), msg: String) {
  let [Token2(_, inf), ..] = src
  let #(a, srcrest) = case src {
    [] -> #(token.EOF, [])
    [Token2(a, _)] -> #(a, [])
    [Token2(a, _), ..srcrest] -> #(a, srcrest)
  }

  let #(b, exrest) = case ex {
    [] -> #(token.EOF, [])
    [b] -> #(b, [])
    [b, ..exrest] -> #(b, exrest)
  }

  case a == b {
    False -> {
      raise_parse_error(
        src,
        itype.Expected(inf, msg, itype.token_to_str(b), itype.token_to_str(a)),
      )
      Error("unmatch")
    }

    _ ->
      case #(srcrest, exrest) {
        #([], _) | #(_, []) -> Ok(srcrest)
        _ -> {
          expect(srcrest, exrest, msg)
          Error("unmatch")
        }
      }
  }
}

pub fn expr_(e: Expr2, src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    [Token2(token.Less, pos), ..rest]
    | [Token2(token.LessEqual, pos), ..rest]
    | [Token2(token.Greater, pos), ..rest]
    | [Token2(token.GreaterEqual, pos), ..rest]
    | [Token2(token.NotEqual, pos), ..rest]
    | [Token2(token.EqualEqual, pos), ..rest]
    | [Token2(token.VbarVbar, pos), ..rest]
    | [Token2(token.AmperAmper, pos), ..rest]
    | [Token2(token.NotEqual, pos), ..rest] -> {
      let [Token2(binop, inf), ..] = src
      let #(e2, rest3) = expr2(rest)
      expr_(
        Expr2(BinOp(t: TUnknown, name: binop, left: e, right: e2), inf),
        rest3,
      )
    }
    _ -> #(e, src)
  }
}

pub fn expr(src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    [] -> #(Expr2(NOP, eofinf()), [])
    _ -> {
      let #(e, rest) = expr2(src)
      expr_(e, rest)
    }
  }
}

pub fn expr2_(e: Expr2, src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    [Token2(token.PlusPlus, pos), ..rest]
    | [Token2(token.MinusMinus, pos), ..rest]
    | [Token2(token.Plus, pos), ..rest]
    | [Token2(token.Minus, pos), ..rest] -> {
      let [Token2(binop, inf), ..] = src
      let #(e2, rest3) = term(rest)
      expr2_(
        Expr2(BinOp(t: TUnknown, name: binop, left: e, right: e2), inf),
        rest3,
      )
    }
    _ -> #(e, src)
  }
}

pub fn expr2(src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    [] -> #(Expr2(NOP, eofinf()), [])
    _ -> {
      let #(e, rest) = term(src)
      expr2_(e, rest)
    }
  }
}

pub fn term_(e: Expr2, src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    // RecordUpdate(..Record, name:val, nave:val)
    [Token2(token.LeftParen, inf), Token2(token.DotDot, _), ..rest] -> {
      let #(spread, rest2) = expr(rest)
      case rest2 {
        [Token2(token.Comma, _), ..rest3] -> {
          let #(rec_update_args, rest4) = read_record_update_args(rest3, [])
          #(
            Expr2(
              itype.RecordUpdate(
                constructor: e,
                spread: Some(itype.RecordUpdateSpread(base: spread)),
                arguments: rec_update_args,
              ),
              inf,
            ),
            rest4,
          )
        }
      }
    }

    [Token2(token.LeftParen, inf), ..rest] ->
      case e {
        // RecordUpdate Record(name:Val, name:Val)
        Expr2(itype.TypeName(_), _) | Expr2(TypeAccess(_, _expr, _member), _) -> {
          let #(rec_update_args, rest2) = read_record_update_args(rest, [])
          #(
            Expr2(
              itype.RecordUpdate(
                constructor: e,
                spread: None,
                arguments: rec_update_args,
              ),
              inf,
            ),
            rest2,
          )
        }
        _ -> {
          let #(params, rest2) = read_funcall_params_until_paren(rest, [])
          //log("%%%% call ~p ~p", [untyped(e), untyped(rest2)])
          #(
            Expr2(itype.Funcall(t: TUnknown, fun: e, params: params), inf),
            rest2,
          )
        }
      }

    [Token2(token.Star, _), ..rest] | [Token2(token.Slash, _), ..rest] -> {
      let [Token2(binop, inf), ..] = src
      let #(e2, rest3) = factor(rest)
      term_(
        Expr2(itype.BinOp(t: TUnknown, name: binop, left: e, right: e2), inf),
        rest3,
      )
    }
    _ -> #(e, src)
  }
}

pub fn term(src: List(Token2)) -> #(Expr2, List(Token2)) {
  case src {
    [] -> #(Expr2(NOP, eofinf()), [])
    _ -> {
      let #(e, rest) = factor(src)
      term_(e, rest)
    }
  }
}

// name: Value, Value
//pub fn read_record_update_args(src:List(Token2),acc:List(itype.RecordUpdateArg)) -> Tuple(List(itype.RecordUpdateArg),List(Token2)) {
pub fn read_record_update_args(
  src: List(Token2),
  acc: List(itype.RecordUpdateArg),
) -> #(List(itype.RecordUpdateArg), List(Token2)) {
  let [Token2(_, pos), ..] = src
  let #(ua, rest3) = case src {
    [Token2(token.Name(name), inf), Token2(token.Colon, _), ..rest] -> {
      let #(expr, rest2) = expr(rest)
      #(itype.RecordUpdateArg(label: Some(name), value: expr), rest2)
    }
    _ -> {
      let #(expr, rest2) = expr(src)
      #(itype.RecordUpdateArg(label: None, value: expr), rest2)
    }
  }
  let newacc = [ua, ..acc]
  case rest3 {
    [Token2(token.RightParen, _), ..rest4] -> #(list.reverse(newacc), rest4)

    [Token2(token.Comma, _), ..rest4] -> read_record_update_args(rest4, newacc)
  }
}

// name:Value, Value
fn read_param(src: List(Token2)) {
  case src {
    // name:Expr
    [Token2(token.Name(name), _), Token2(token.Colon, _), ..rest] -> {
      let #(e, rest2) = expr(rest)
      #(DefParam(t: TUnknown, name: Some(name), expr: e), rest2)
    }

    // Expr
    _ -> {
      let #(e, rest2) = expr(src)
      #(DefParam(t: TUnknown, name: None, expr: e), rest2)
    }
  }
}

pub fn read_funcall_params_until_paren(src: List(Token2), acc: List(DefParam)) {
  //log("read_funcall_params_until_paren ~p",[src])
  case src {
    [Token2(token.RightParen, _)] -> #([], [])
    [Token2(token.RightParen, _), ..rest0] -> #([], rest0)
    _ -> {
      let #(prm, rest2) = read_param(src)
      let newacc = [prm, ..acc]
      case rest2 {
        [Token2(token.RightParen, _), ..rest3] -> #(list.reverse(newacc), rest3)
        [Token2(token.Comma, _), ..rest3] ->
          read_funcall_params_until_paren(rest3, newacc)
      }
    }
  }
}

pub fn read_list_paren_(src: List(Token2), acc: List(Expr2)) {
  let #(e, rest2) = expr(src)
  let newacc = [e, ..acc]
  read_list_paren(rest2, newacc)
}

pub fn read_list_paren(src: List(Token2), newacc: List(Expr2)) {
  case src {
    [] -> {
      exception.throw_ex(itype.UnexpectedEOF)
      #(#(newacc, None), [])
    }
    [
      Token2(token.Comma, _),
      Token2(token.DotDot, inf),
      Token2(token.RightParen, _),
      ..rest3
    ] -> #(#(list.reverse(newacc), Some(token.DiscardName("unnamed"))), rest3)

    [
      Token2(token.Comma, _),
      Token2(token.DotDot, inf),
      Token2(token.Name(tailname), _),
      Token2(token.RightParen, _),
      ..rest3
    ] -> #(#(list.reverse(newacc), Some(token.Name(tailname))), rest3)

    [Token2(token.RightParen, _), ..rest3] -> #(
      #(list.reverse(newacc), None),
      rest3,
    )

    [Token2(token.Comma, _), ..rest3] -> read_list_paren_(rest3, newacc)

    _ -> read_list_paren_(src, newacc)
  }
}

fn read_list_square_(src: List(Token2), acc: List(Expr2)) {
  let #(e, rest2) = expr(src)
  let newacc = [e, ..acc]
  read_list_square(rest2, newacc)
}

fn read_list_square(src: List(Token2), acc: List(Expr2)) {
  case src {
    [] -> {
      exception.throw_ex(itype.UnexpectedEOF)
      #(#(acc, None), [])
    }
    [
      Token2(token.Comma, _),
      Token2(token.DotDot, inf),
      Token2(token.RightSquare, _),
      ..rest3
    ] -> #(#(list.reverse(acc), Some(token.DiscardName("unnamed"))), rest3)

    [
      Token2(token.Comma, _),
      Token2(token.DotDot, inf),
      Token2(token.Name(tailname), _),
      Token2(token.RightSquare, _),
      ..rest3
    ] -> #(#(list.reverse(acc), Some(token.Name(tailname))), rest3)

    [Token2(token.RightSquare, _), ..rest3] -> #(
      #(list.reverse(acc), None),
      rest3,
    )

    [Token2(token.Comma, _), ..rest3] -> read_list_square_(rest3, acc)

    _ -> read_list_square_(src, acc)
  }
}

pub fn read_block(src: List(Token2), acc: List(Expr2)) {
  //log("read_block ~p : ~p",[untyped(src),untyped(acc)])
  let #(e, rest2) = expr(src)
  //log("expr = ~p",[e])
  let newacc = [e, ..acc]
  case rest2 {
    //[] -> #(list.reverse(newacc),[])
    [Token2(token.RightBrace, _), ..rest3] -> #(list.reverse(newacc), rest3)
    _ -> read_block(rest2, newacc)
  }
}

pub fn read_block_paren(src: List(Token2), acc: List(Expr2)) {
  let #(e, rest2) = expr(src)
  let newacc = [e, ..acc]
  case rest2 {
    [Token2(token.RightParen, _), ..rest3] -> #(list.reverse(newacc), rest3)
    _ -> read_block(rest2, newacc)
  }
}

// Expr -> Expr
pub fn read_cond_clauses_(src: List(Token2), acc: List(CondClause)) {
  case src {
    //[Token2(token.Comma, pos), ..rest3] -> read_cond_clauses(rest3, acc)
    [Token2(token.RightBrace, _), ..rest5] -> #(list.reverse(acc), rest5)
    _ -> read_cond_clauses(src, acc)
  }
}

pub fn read_cond_clauses(src: List(Token2), acc: List(CondClause)) {
  let #(subj, rest2) = expr(src)
  case rest2 {
    [Token2(token.RArrow, pos), ..rest3] -> {
      let #(thenexpr, rest4) = expr(rest3)
      read_cond_clauses_(rest4, [
        CondClause(t: TUnknown, expr: subj, then: thenexpr),
        ..acc
      ])
    }
  }
}

fn read_if(src: List(Token2), acc: List(CondClause)) {
  let #(subj, rest2) = expr(src)
  let #(then_expr, rest3) = expr(rest2)
  let cond_clause = CondClause(t: TUnknown, expr: subj, then: then_expr)
  let new_acc = [cond_clause, ..acc]
  case rest3 {
    [Token2(token.ElseIf, _), ..rest4] -> read_if(rest4, new_acc)
    [Token2(token.Else, _), ..rest4] -> {
      let #(else_expr, rest5) = expr(rest4)
      let Expr2(_, inf) = else_expr
      let cond_clause =
        CondClause(t: TUnknown, expr: Expr2(Boolean(True), inf), then: else_expr,
        )
      #(list.reverse([cond_clause, ..acc]), rest5)
    }
  }
}

pub fn read_case_clauses(src: List(Token2), acc: List(CaseClause)) {
  //log("read_clause ~p : ~p",[untyped(src),untyped(acc)])
  let #(casepattern, rest2) = expr(src)
  case rest2 {
    [Token2(token.RArrow, pos), ..rest3] -> {
      let #(thenexpr, rest4) = expr(rest3)
      let newacc = [
        CaseClause(
          t: TUnknown,
          pattern: casepattern,
          guard: None,
          then: thenexpr,
        ),
        ..acc
      ]
      case rest4 {
        //[] -> #(list.reverse(newacc),[])
        [Token2(token.RightBrace, _), ..rest5] -> #(list.reverse(newacc), rest5)
        _ -> read_case_clauses(rest4, newacc)
      }
    }
  }
}

fn read_defargs__(src: List(Token2), acc: List(DefArg)) {
  case src {
    [Token2(token.RightParen, _), ..rest] -> #(list.reverse(acc), rest)
    [Token2(token.Comma, _), ..rest] -> read_defargs_(rest, acc)
    [Token2(invalid_token, inf), ..rest] -> {
      log("defarg error: ~p", [src])
      raise_parse_error(
        src,
        itype.ErrorMsg(
          inf,
          "invalid arg: ",
          dynamic.from(itype.token_to_str(invalid_token)),
        ),
      )
      read_defargs_(src, acc)
    }
  }
  // dummy
}

//DefArg(t: T, name: String, default: Option(Expr))
fn read_defargs_(src: List(Token2), acc: List(DefArg)) {
  case read_defarg(src) {
    #(opt_arg, rest) ->
      case opt_arg {
        Some(arg) -> read_defargs__(rest, [arg, ..acc])
        None -> #(list.reverse(acc), rest)
      }
  }
}

fn read_defarg(src: List(Token2)) -> #(Option(DefArg), List(Token2)) {
  case src {
    [Token2(token.RightParen, _), ..rest] -> #(None, rest)

    [Token2(token.Name(name), _), Token2(token.Colon, _), ..rest] -> {
      let #(argtype, rest2) = expr(rest)
      let tt = itype.get_type2(argtype)
      case rest2 {
        [Token2(token.Equal, _), ..rest3] -> {
          let #(default_value, rest4) = expr(rest3)
          #(
            Some(DefArg(
              t: tt,
              name: name,
              renamed: "",
              default: Some(default_value),
            )),
            rest4,
          )
        }
        _ -> #(
          Some(DefArg(t: tt, name: name, renamed: "", default: None)),
          rest2,
        )
      }
    }

    [Token2(token.Name(name), _), Token2(token.Equal, _), ..rest] -> {
      let #(default_value, rest2) = expr(rest)
      #(
        Some(DefArg(
          t: TUnknown,
          name: name,
          renamed: "",
          default: Some(default_value),
        )),
        rest2,
      )
    }
    [Token2(token.Name(name), _), ..rest] -> #(
      Some(DefArg(t: TUnknown, name: name, renamed: "", default: None)),
      rest,
    )
  }
}

fn read_defargs(src: List(Token2)) {
  read_defargs_(src, [])
}

//fn read_argtypes(src:List(Token2), acc:List(ArgType)) {
//    case src {
//        [ Token2(token.RightParen, _ ), ..rest ] -> #( list.reverse(acc), rest )
//
//        [ Token2(token.Name(typename), _), ..rest ] -> {
//            read_argtypes_(rest,[ ArgType(t:itype.type_from_typename(typename)), ..acc])
//        }
//
//        [ Token2(token.UpName(typename), _), ..rest ] -> {
//            read_argtypes_(rest,[ ArgType(t:itype.type_from_typename(typename)), ..acc])
//        }
//    }
//}
fn read_argtypes(
  src: List(Token2),
  acc: List(T),
) -> #(List(T), List(Token2), token.Token) {
  case src {
    [Token2(token.Equal, _), ..rest] -> #(list.reverse(acc), rest, token.Equal)
    [Token2(token.RightParen, _), ..rest] -> #(
      list.reverse(acc),
      rest,
      token.RightParen,
    )

    // typename(
    [
      Token2(token.Name(tname), _) as typename,
      Token2(token.LeftParen, _),
      ..rest
    ] -> {
      let #(arg_ts, rest2, _) = read_argtypes(rest, [])
      let tt = TProxyType(tname, arg_ts)
      read_argtypes_(rest2, [tt, ..acc])
    }

    // typename
    [Token2(token.Name(tname), _) as typename, ..rest] ->
      read_argtypes_(rest, [TProxyType(tname, []), ..acc])

    // TypeName(
    [
      Token2(token.UpName(tname), _) as typename,
      Token2(token.LeftParen, _),
      ..rest
    ] -> {
      let #(arg_ts, rest2, _) = read_argtypes(rest, [])
      let tt = TCustomType(tname, arg_ts)
      read_argtypes_(rest2, [tt, ..acc])
    }
    // TypeName
    [Token2(token.UpName(tname), _) as typename, ..rest] ->
      read_argtypes_(rest, [TCustomType(tname, []), ..acc])
    // List(
    [Token2(token.List, _) as typename, Token2(token.LeftParen, _), ..rest] -> {
      let #(arg_ts, rest2, _) = read_argtypes(rest, [])
      let assert [argt] = arg_ts
      let tt = TList(argt)
      read_argtypes_(rest2, [tt, ..acc])
    }
  }
}

fn read_argtypes_(
  src: List(Token2),
  acc: List(T),
) -> #(List(T), List(Token2), token.Token) {
  case src {
    [Token2(token.Equal, _), ..rest] -> #(list.reverse(acc), rest, token.Equal)
    [Token2(token.RightParen, _), ..rest] -> #(
      list.reverse(acc),
      rest,
      token.RightParen,
    )
    [Token2(token.Comma, _), ..rest] -> read_argtypes_(rest, acc)
  }
}

fn read_type_list(src: List(Token2), acc: List(T)) -> #(List(T), List(Token2)) {
  case src {
    [Token2(token.RightParen, _), ..rest] -> #(list.reverse(acc), rest)
    [Token2(token.Comma, _), ..rest] -> read_type_list(rest, acc)

    _ -> {
      let #(t, rest) = read_type(src)
      read_type_list(rest, [t, ..acc])
    }
  }
}

fn read_type(src: List(Token2)) -> #(T, List(Token2)) {
  case src {
    // #(T,T(T,T),T)
    [Token2(token.Hash, _), Token2(token.LeftParen, _), ..rest] -> {
      let #(tlst, rest2) = read_type_list(rest, [])
      #(TTuple(tlst), rest2)
    }
    // (T,T(T,T),T)
    [Token2(token.List, _), Token2(token.LeftParen, _), ..rest] -> {
      let #([t], rest2) = read_type_list(rest, [])
      #(TList(t), rest2)
    }
    // Upname(T,T)
    [Token2(token.UpName(name), _), Token2(token.LeftParen, _), ..rest] -> {
      let #(tlst, rest2) = read_type_list(rest, [])
      #(TCustomType(name, tlst), rest2)
    }
    [Token2(token.UpName(name), _), ..rest] -> #(TCustomType(name, []), rest)

    // name
    [Token2(token.Name(name), _), ..rest] -> #(
      itype.type_from_typename(name),
      rest,
    )

    // Upname
    [Token2(token.UpName(name), _), ..rest] -> #(
      itype.type_from_typename(name),
      rest,
    )
  }
}

pub fn read_record_argdef_paren(
  src: List(Token2),
  acc: List(RecordConstructorArg),
) {
  let #(rec_arg, rest2) = case src {
    [Token2(token.Name(name), _), Token2(token.Colon, _), ..rest]
    | [Token2(token.Name(name), _), Token2(token.Colon, _), ..rest] -> {
      let #(typename, rest3) = read_type(rest)
      #(RecordConstructorArg(label: Some(name), t: typename), rest3)
    }

    _ -> {
      let #(typename, rest3) = read_type(src)
      #(RecordConstructorArg(label: None, t: typename), rest3)
    }
  }

  let newacc = [rec_arg, ..acc]
  case rest2 {
    [Token2(token.RightParen, _), ..rest4] -> #(list.reverse(newacc), rest4)

    [Token2(token.Comma, _), ..rest4] -> read_record_argdef_paren(rest4, newacc)
  }
}

pub fn make_label_to_index(args: List(RecordConstructorArg)) {
  let seq = list.range(2, list.length(args) + 2)
  let zipped: List(#(Int, RecordConstructorArg)) = list.zip(seq, args)
  let mm =
    list.fold(zipped, map.new(), fn(mm, e: #(Int, RecordConstructorArg)) {
      let #(idx, rarg) = e
      case rarg.label {
        Some(label) -> map.insert(mm, label, idx)
        None -> mm
      }
    })
  mm
}

pub fn read_record_constructor_block(
  src: List(Token2),
  acc: List(RecordConstructor),
) {
  case src {
    [Token2(token.UpName(typename), pos), Token2(token.LeftParen, _), ..rest] -> {
      let #(args, rest2) = read_record_argdef_paren(rest, [])
      let newacc = [
        RecordConstructor(
          name: typename,
          arguments: args,
          label_to_index: make_label_to_index(args),
        ),
        ..acc
      ]
      case rest2 {
        [Token2(token.RightBrace, _), ..rest3] -> #(list.reverse(newacc), rest3)
        _ -> read_record_constructor_block(rest2, newacc)
      }
    }

    [Token2(token.UpName(typename), pos), ..rest2] -> {
      let newacc = [
        RecordConstructor(
          name: typename,
          arguments: [],
          label_to_index: map.new(),
        ),
        ..acc
      ]
      case rest2 {
        [Token2(token.RightBrace, _), ..rest3] -> #(list.reverse(newacc), rest3)
        _ -> read_record_constructor_block(rest2, newacc)
      }
    }
  }
}

pub fn factor(src: List(Token2)) {
  case src {
    [Token2(token.EOF, _)] -> #(nopexpr2(), [])
    [Token2(token.CommentModule(_) as comment, inf), ..rest]
    | [Token2(token.CommentDoc(_) as comment, inf), ..rest]
    | [Token2(token.CommentNormal(_) as comment, inf), ..rest]
    | [Token2(token.BlockCommentModule(_) as comment, inf), ..rest]
    | [Token2(token.BlockCommentDoc(_) as comment, inf), ..rest]
    | [Token2(token.BlockCommentNormal(_) as comment, inf), ..rest] -> {
      log("after comment ~p", [rest])
      factor_(Expr2(itype.Comment(comment), inf), rest)
    }

    _ -> factor_(nopexpr2(), src)
  }
}

fn read_import_modules_(src: List(Token2), acc: List(String)) {
  case src {
    [Token2(token.Slash, _), ..rest] -> read_import_modules(rest, acc)
    _ -> {
      let [package, ..modules] = acc
      #(list.reverse(modules), package, src)
    }
  }
}

fn read_import_modules(src: List(Token2), acc: List(String)) {
  case src {
    [Token2(token.Name(name), _), ..rest] ->
      read_import_modules_(rest, [name, ..acc])
  }
}

fn read_import_as(module: List(String), package: String, src: List(Token2)) {
  case src {
    [Token2(token.Name(as_name), inf), ..rest] -> #(
      Expr2(ImportAs(module: module, package: package, as_name: as_name), inf),
      rest,
    )
  }
}

fn read_unqualified_list_(src: List(Token2), acc: List(UnqualifiedImport)) {
  case src {
    [Token2(token.RightBrace, _), ..rest] -> #(list.reverse(acc), rest)
    [Token2(token.Comma, _), ..rest] -> read_unqualified_list(rest, acc)
  }
}

fn read_unqualified_list(src: List(Token2), acc: List(UnqualifiedImport)) {
  case src {
    [
      Token2(token.Name(name), _),
      Token2(token.As, _),
      Token2(token.Name(as_name), _),
      ..rest
    ] ->
      read_unqualified_list_(rest, [
        UnqualifiedImportVal(name: name, as_name: Some(as_name)),
        ..acc
      ])
    [Token2(token.Name(name), _), ..rest] ->
      read_unqualified_list_(rest, [
        UnqualifiedImportVal(name: name, as_name: None),
        ..acc
      ])

    [
      Token2(token.UpName(name), _),
      Token2(token.As, _),
      Token2(token.UpName(as_name), _),
      ..rest
    ] ->
      read_unqualified_list_(rest, [
        UnqualifiedImportType(name: name, as_name: Some(as_name)),
        ..acc
      ])
    [Token2(token.UpName(name), _), ..rest] ->
      read_unqualified_list_(rest, [
        UnqualifiedImportType(name: name, as_name: None),
        ..acc
      ])
  }
}

fn read_import_(module: List(String), package: String, src: List(Token2)) {
  case src {
    [Token2(token.LeftBrace, inf), ..rest] -> {
      let #(unqualified_import_list, rest2) = read_unqualified_list(rest, [])
      #(
        Expr2(
          ImportUnqualified(
            module: module,
            package: package,
            unqualified: unqualified_import_list,
          ),
          inf,
        ),
        rest,
      )
    }
  }
}

fn srcinf_of_srclist(src: List(Token2)) {
  case src {
    [Token2(_, inf)] -> inf
    [Token2(_, inf), ..] -> inf
  }
}

fn read_import(src: List(Token2)) {
  let inf = srcinf_of_srclist(src)
  let #(modules, package, rest) = read_import_modules(src, [])

  //log("import ~p ~p ~p",[untyped(modules),untyped(package),untyped(rest)])
  case rest {
    [Token2(token.As, _), ..rest2] -> read_import_as(modules, package, rest2)
    [Token2(token.Dot, _), ..rest2] -> read_import_(modules, package, rest2)
    _ -> #(Expr2(Import(module: modules, package: package), inf), rest)
  }
}

pub fn factor_(eprev: Expr2, src: List(Token2)) -> #(Expr2, List(Token2)) {
  let #(public_flag, rest_a) = case src {
    [Token2(token.Pub, pos), ..rest_b] -> #(True, rest_b)
    _ -> #(False, src)
  }

  //log("### ~p] ~p",[untyped(eprev),untyped(rest_a)])
  case rest_a {
    // import module/module.package as as_name
    [Token2(token.Import, _), ..rest] -> read_import(rest)

    // const
    [
      Token2(token.Const, _),
      constname,
      Token2(token.Colon, _),
      Token2(token.UpName(typename), inf),
      Token2(token.Equal, _),
      ..rest
    ] -> {
      let #(exprname, _) = expr([constname])
      let #(expr, rest2) = expr(rest)
      #(
        Expr2(
          DefConst(t: TUnknown, public: public_flag, name: exprname, value: expr,
          ),
          inf,
        ),
        rest2,
      )
    }

    // const
    [Token2(token.Const, _), constname, Token2(token.Equal, _), ..rest] -> {
      let #(exprname, _) = expr([constname])
      let #(expr, rest2) = expr(rest)
      let Expr2(_, inf) = exprname
      #(
        Expr2(
          DefConst(t: TUnknown, public: public_flag, name: exprname, value: expr,
          ),
          inf,
        ),
        rest2,
      )
    }

    //        [Token2(token.Const, pos), name_token, Token2(token.Equal, _), ..rest] -> {
    //            let #(name,_) = term([name_token])
    //            let #(val, rest2) = term(rest)
    //            case eprev {
    //                Comment(c) -> {
    //                    log("const with comment ~p = ~p",[name,val])
    //                    #( Const(t: TUnknown, name:name, value:val), rest2)
    //                }
    //                _ -> {
    //                    log("const without comment ~p = ~p",[name,val])
    //                    #( Const(t: TUnknown, name:name, value:val), rest2)
    //                }
    //            }
    //        }
    // defun 
    [
      Token2(token.Fn, _),
      Token2(token.Name(fnname), inf),
      Token2(token.LeftParen, _),
      ..rest
    ] ->
      block("defun", fnname, fn() {
        //log("defun ~s",[untyped(fnname)])
        let #(args, rest2) = read_defargs(rest)
        let #(rtype, rest4) = case rest2 {
          [Token2(token.RArrow, _), ..rest3] -> expr(rest3)
          _ -> #(nopexpr2(), rest2)
        }
        let #(expr, rest5) = expr(rest4)
        //log("defun expr~p",[untyped(expr)])
        #(
          Expr2(
            Defun(
              t: TUnknown,
              public: public_flag,
              name: fnname,
              args: args,
              body: expr,
            ),
            inf,
          ),
          rest5,
        )
      })

    // unnamed fun, unnamed lambda
    [Token2(token.Lambda, inf), Token2(token.LeftParen, _), ..rest]
    | [Token2(token.Fn, inf), Token2(token.LeftParen, _), ..rest] -> {
      //log("defun ~s",[untyped(fnname)])
      let #(args, rest2) = read_defargs(rest)
      let #(rtype, rest4) = case rest2 {
        [Token2(token.RArrow, _), ..rest3] -> expr(rest3)
        _ -> #(nopexpr2(), rest2)
      }
      let #(expr, rest5) = expr(rest4)
      //log("defun expr~p",[untyped(expr)])
      #(Expr2(Fun(t: TUnknown, name: None, args: args, body: expr), inf), rest5)
    }

    // named lambda
    [
      Token2(token.Lambda, inf),
      Token2(token.Name(fnname), _),
      Token2(token.LeftParen, _),
      ..rest
    ] -> {
      //log("defun ~s",[untyped(fnname)])
      let #(args, rest2) = read_defargs(rest)
      let #(rtype, rest4) = case rest2 {
        [Token2(token.RArrow, _), ..rest3] -> expr(rest3)
        _ -> #(nopexpr2(), rest2)
      }
      let #(expr, rest5) = expr(rest4)
      //log("defun expr~p",[untyped(expr)])
      #(
        Expr2(Fun(t: TUnknown, name: Some(fnname), args: args, body: expr), inf),
        rest5,
      )
    }
    // TypeAlias
    // [pub] type Type = Type 
    [
      Token2(token.Type, _),
      Token2(token.UpName(aliasname), inf),
      Token2(token.Equal, _),
      ..rest
    ] -> {
      //log(
      //  "-------------------- TYPE Alias -----------------------------------",
      //  [],
      //)
      let #(type_, rest2) = expr(rest)
      //log("#### TypeAlias: ~s ~p", [untyped(aliasname), untyped(type_)])
      #(
        Expr2(
          DefTypeAlias(
            t: itype.get_type2(type_),
            public: public_flag,
            alias: aliasname,
            parameters: None,
          ),
          inf,
        ),
        rest2,
      )
    }

    // [pub] type CustomType(t) {}
    [
      Token2(token.Type, _),
      Token2(token.UpName(typename), inf),
      Token2(token.LeftParen, _),
      ..rest
    ] -> {
      //log(
      //  "-------------------- CUSTOM TYPE() -----------------------------------",
      //  [],
      //)
      let #(params, rest2) = read_record_argdef_paren(rest, [])
      //log("CustomType ~s ~p", [untyped(typename), untyped(params)])
      case rest2 {
        [Token2(token.LeftBrace, _), ..rest3] -> {
          let #(types, rest4) = read_record_constructor_block(rest3, [])
          //log(
          //  "#### CustomType: ~s(~p) ~p",
          //  [untyped(typename), untyped(params), untyped(types)],
          //)
          #(
            Expr2(
              DefCustomType(
                public: public_flag,
                name: typename,
                parameters: Some(params),
                constructors: types,
              ),
              inf,
            ),
            rest4,
          )
        }
        _ -> #(
          Expr2(
            DefCustomType(
              public: public_flag,
              name: typename,
              parameters: Some(params),
              constructors: [],
            ),
            inf,
          ),
          rest2,
        )
      }
    }
    // [pub] type CustomType {}
    [
      Token2(token.Type, _),
      Token2(token.UpName(typename), inf),
      Token2(token.LeftBrace, _),
      ..rest
    ] -> {
      //log(
      //  "-------------------- CUSTOM TYPE -----------------------------------",
      //  [],
      //)
      let #(types, rest2) = read_record_constructor_block(rest, [])
      //log("#### CustomType: ~s ~p", [untyped(typename), untyped(types)])
      #(
        Expr2(
          DefCustomType(
            public: public_flag,
            name: typename,
            parameters: None,
            constructors: types,
          ),
          inf,
        ),
        rest2,
      )
    }

    // [pub] type CustomType
    [Token2(token.Type, _), Token2(token.UpName(typename), inf), ..rest] -> //log(
    //  "-------------------- CUSTOM TYPE -----------------------------------",
    //  [],
    //)
    #(
      Expr2(
        DefCustomType(
          public: public_flag,
          name: typename,
          parameters: None,
          constructors: [],
        ),
        inf,
      ),
      rest,
    )

    //        // external fn
    //        [Token2(token.External, _), Token2(token.Fn, _), Token2(token.Name(fnname), _), Token2(token.LeftParen, _), ..rest2 ] -> {
    //                let #(args, rest3) = read_argtypes(rest2,[])
    //                case rest3 {
    //                    [ Token2(token.RArrow, _), Token2(token.UpName(rettype), _), Token2(token.Equal, _), Token2(token.String(exmodulename), _), Token2(token.String(exfnname), _), ..rest4 ] -> {
    //                        #(ExternalFun(t: itype.type_from_typename(rettype), public: public_flag, name: fnname, module: exmodulename, fun: exfnname, argtype:args), rest4 )
    //                    }
    //                }
    // external fn
    [
      Token2(token.External, _),
      Token2(token.Fn, _),
      Token2(token.Name(fnname), inf),
      Token2(token.LeftParen, _),
      ..rest2
    ] -> {
      log("ast external_fn", [])
      let #(args, rest3, _) = read_argtypes(rest2, [])
      log("ast external_fn end", [])
      case rest3 {
        [Token2(token.RArrow, _), ..rest4] -> {
          let assert #([rarg], rest5, endmark) = read_argtypes(rest4, [])
          let assert endmark = token.Equal
          case rest5 {
            [
              Token2(token.String(exmodulename), _),
              Token2(token.String(exfnname), _),
              ..rest6
            ] -> #(
              Expr2(
                DefExternalFun(
                  t: rarg,
                  public: public_flag,
                  name: fnname,
                  module: exmodulename,
                  fun: exfnname,
                  argtype: args,
                ),
                inf,
              ),
              rest6,
            )
          }
        }
      }
    }

    // external type
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.UpName(typename), inf),
      Token2(token.LeftParen, _),
      ..rest2
    ] -> #(
      Expr2(
        DefExternalType(public: public_flag, name: typename, arguments: []),
        inf,
      ),
      rest2,
    )
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.Name(typename), inf),
      ..rest2
    ] -> #(
      Expr2(
        DefExternalType(public: public_flag, name: typename, arguments: []),
        inf,
      ),
      rest2,
    )
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.UpName(typename), inf),
      ..rest2
    ] -> #(
      Expr2(
        DefExternalType(public: public_flag, name: typename, arguments: []),
        inf,
      ),
      rest2,
    )

    [Token2(token.Set, pos), ..rest] -> {
      let #(pattern, rest2) = expr(rest)
      let Expr2(_, inf) = pattern
      case rest2 {
        [Token2(token.ColonEqual, _), ..rest3] -> {
          let #(val, rest4) = expr(rest3)
          #(
            Expr2(itype.Set(t: TUnknown, pattern: pattern, value: val), inf),
            rest4,
          )
        }
        [Token2(t, _), ..rest3] -> {
          raise_parse_error(
            rest3,
            Expected(
              inf,
              msg: "",
              a: itype.token_to_str(token.ColonEqual),
              b: itype.token_to_str(t),
            ),
          )
          #(nopexpr2(), rest3)
        }
      }
    }

    // let PATTERN = EXPR
    // let [a,b,c] = EXPR
    // let #(a,b,c) = EXPR
    // let Record(a,b) = EXPR
    [Token2(token.Let, pos), ..rest] -> {
      let #(pattern, rest2) = expr(rest)
      let Expr2(_, inf) = pattern
      case rest2 {
        [Token2(token.Equal, _), ..rest3] -> {
          let #(val, rest4) = expr(rest3)
          #(
            Expr2(itype.Let(t: TUnknown, pattern: pattern, value: val), inf),
            rest4,
          )
        }
      }
    }

    // case EXPR {
    //      PATTERN -> EXPR   
    //      PATTERN -> EXPR   
    // } 
    [Token2(token.Case, inf), ..rest] -> {
      let #(subj, rest2) = expr(rest)
      //log(
      //  "CASE ~p ----------------------------------------------------",
      //  [subj],
      //)
      let [Token2(token.LeftBrace, _), ..rest3] = rest2
      let #(clauses, rest4) = read_case_clauses(rest3, [])
      #(
        Expr2(itype.Case(t: TUnknown, subjects: subj, clauses: clauses), inf),
        rest4,
      )
    }

    // when EXPR { BODY }
    [Token2(token.When, inf), ..rest] -> {
      let #(subj, rest2) = expr(rest)
      let [Token2(token.LeftBrace, _), ..rest3] = rest2
      let #(body, rest4) = read_block(rest3, [])
      #(Expr2(itype.When(subjects: subj, body: body), inf), rest4)
    }

    // if EXPR THEN_EXPR else ELSE_EXPR
    [Token2(token.If, inf), ..rest] -> {
      let #(clauses, rest2) = read_if(rest, [])
      #(Expr2(itype.Cond(t: TUnknown, clauses: clauses), inf), rest2)
    }

    // cond {
    //    Expr -> Expr
    //    Expr -> { Expr }
    // }
    [Token2(token.Cond, inf), Token2(token.LeftBrace, _), ..rest] -> {
      let #(clauses, rest4) = read_cond_clauses(rest, [])
      #(Expr2(itype.Cond(t: TUnknown, clauses: clauses), inf), rest4)
    }

    // {
    [Token2(token.LeftBrace, inf), ..rest] -> {
      let #(listcontent, rest2) = read_block(rest, [])
      //log("block ~p",[listcontent])
      #(Expr2(itype.Block(t: TUnknown, expressions: listcontent), inf), rest2)
    }
    [Token2(token.LeftParen, inf), ..rest] -> {
      let #(listcontent, rest2) = read_block_paren(rest, [])
      //log("block ~p",[listcontent])
      #(
        Expr2(itype.Sequence(t: TUnknown, expressions: listcontent), inf),
        rest2,
      )
    }
    [Token2(token.LeftSquare, inf), ..rest] -> {
      let #(#(exprs, listtail), rest2) = read_list_square(rest, [])
      case listtail {
        Some(tail) -> #(
          Expr2(
            itype.ListExWithTail(t: TUnknown, elements: exprs, tail: tail),
            inf,
          ),
          rest2,
        )
        None -> #(Expr2(itype.ListEx(t: TUnknown, elements: exprs), inf), rest2)
      }
    }
    [Token2(token.Hash, inf), Token2(token.LeftParen, _), ..rest] -> {
      let #(#(exprs, None), rest2) = read_list_paren(rest, [])
      #(Expr2(itype.Tuple(elements: exprs), inf), rest2)
    }

    [Token2(token.Sigil(name), inf), ..rest] -> {
      let #(expr, rest2) = expr(rest)
      #(Expr2(itype.SigilExpr(name, expr), inf), rest2)
    }

    [Token2(token.Quote, inf), ..rest] -> {
      let #(expr, rest2) = expr(rest)
      #(Expr2(itype.Quote(expr), inf), rest2)
    }

    [
      Token2(token.Name(_), inf) as name,
      Token2(token.Dot, _),
      Token2(token.UpName(member), _),
      ..rest
    ] -> {
      let #(nameexpr, _) = expr([name])
      //log("ast typeaccess ~p -> ~p", [untyped(name), untyped(nameexpr)])
      #(
        Expr2(itype.TypeAccess(t: TUnknown, expr: nameexpr, member: member), inf,
        ),
        rest,
      )
    }

    [
      Token2(token.Name(_), inf) as name,
      Token2(token.Dot, _),
      Token2(token.Name(member), _),
      ..rest
    ] -> {
      let #(nameexpr, _) = expr([name])
      //log("ast memberaccess ~p -> ~p", [untyped(name), untyped(nameexpr)])
      #(
        Expr2(
          itype.MemberAccess(t: TUnknown, expr: nameexpr, member: member),
          inf,
        ),
        rest,
      )
    }

    [
      Token2(token.Name(_), inf) as name,
      Token2(token.Dot, _),
      Token2(token.Int(member), _),
      ..rest
    ] -> {
      let #(nameexpr, _) = expr([name])
      #(
        Expr2(
          itype.MemberAccess(t: TUnknown, expr: nameexpr, member: member),
          inf,
        ),
        rest,
      )
    }

    [Token2(token.String(val), inf), ..rest] -> #(
      Expr2(itype.StringVal(value: val), inf),
      rest,
    )

    [Token2(token.Name(name), inf), Token2(token.Colon, _), ..rest] -> {
      //let #(typexpr, rest2) = expr(rest)
      let #(ttype, rest2) = read_type(rest)
      #(Expr2(itype.Name(t: ttype, name: name, renamed: None), inf), rest2)
    }

    [Token2(token.Name(name), inf), ..rest] -> #(
      Expr2(itype.Name(t: TUnknown, name: name, renamed: None), inf),
      rest,
    )
    [Token2(token.UpName(name), inf), ..rest] -> #(
      Expr2(itype.TypeName(name: name), inf),
      rest,
    )

    [Token2(token.Int(val), inf), ..rest] -> {
      let assert Ok(intval) = int.parse(val)
      #(Expr2(itype.IntVal(value: intval), inf), rest)
    }
    [Token2(token.Float(val), inf), ..rest] -> {
      let assert Ok(floatval) = float.parse(val)
      #(Expr2(itype.FloatVal(value: floatval), inf), rest)
    }

    // DiscardName
    [Token2(token.DiscardName(name), inf), ..rest] -> #(
      Expr2(itype.DiscardName(t: TUnknown, name: name), inf),
      rest,
    )

    [Token2(token.True, inf), ..rest] -> #(
      Expr2(Boolean(gleam.True), inf),
      rest,
    )
    [Token2(token.False, inf), ..rest] -> #(
      Expr2(Boolean(gleam.False), inf),
      rest,
    )

    //_ -> Error("err")
    //[tkn, ..rest] -> Ok( #(tkn,rest) )
    [Token2(token.EOF, _)] -> //log("[EOF]", [])
    #(nopexpr2(), [])

    [Token2(token.EOF, _), ..rest] -> //log("[EOF, ..rest]", [])
    #(nopexpr2(), [])

    [unknown, ..rest] -> {
      log("unknown factor: ~p", [unknown])
      factor(rest)
    }

    [] -> //log("[] EOF", [])
    #(nopexpr2(), [])
  }
}

pub fn parse_ast(src: List(Token2)) {
  expr(src)
}
//pub fn force_parse_ast(src: List(Token2),acc:List(Expr2) ) {
//  case exception.catch_ex(fn() { expr(src) }) {
//Ok(expr) -> list.reverse([expr, ..acc]) 
//Error(ParseError(rest,err)) -> force_parse_ast(//
//  }
//}
