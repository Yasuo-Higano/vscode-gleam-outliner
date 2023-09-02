import otp/io
import gleam
import lib/pprint.{format}
import gleam_lsp/interpreter/lexer
import gleam_lsp/interpreter/itype.{SrcInf, Token2}
import gleam_lsp/interpreter/token
import lib/log.{log}
import gleam/string
import gleam/list
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/int
import gleam/float
import lib/untyped.{Untyped, untyped}
import sola/exception

fn eofinf() {
  SrcInf(0, 0, None)
}

pub type Range {
  Range(sl: Int, sc: Int, el: Int, ec: Int)
}

fn range(s: SrcInf, e: SrcInf) {
  let SrcInf(sl, sc, _) = s
  let SrcInf(el, ec, _) = e
  Range(sl, sc, el, ec)
}

pub type DocSymbol2 {
  DocSymbol2(DocSymbol, Range)
}

pub type DocSymbol {
  UnknownSymbol
  DefConst(public: Bool, name: String)
  Defun(public: Bool, name: String)
  DefType(public: Bool, name: String)
  DefTypeAlias(public: Bool, name: String)
  Constructor(public: Bool, name: String)
  DefExternalFun(public: Bool, name: String)
  DefExternalType(public: Bool, name: String)
  ParentWithChildren(parent: DocSymbol2, children: List(DocSymbol2))
}

fn read_constructors(src: List(Token2), acc: List(DocSymbol2)) {
  case src {
    [Token2(token.RightBrace, _) as head, ..tail] -> #(list.reverse(acc), tail)
    [Token2(token.UpName(name), infs), Token2(token.LeftParen, infe), ..tail] -> {
      let #(_, tail2) = skip_to_expr_end(tail, 1)
      read_constructors(
        tail2,
        [
          DocSymbol2(Constructor(public: False, name: name), range(infs, infe)),
          ..acc
        ],
      )
    }
    [Token2(token.UpName(name), infs), Token2(_, infe), ..tail] ->
      read_constructors(
        tail,
        [
          DocSymbol2(Constructor(public: False, name: name), range(infs, infe)),
          ..acc
        ],
      )
    [_, ..tail] -> read_constructors(tail, acc)
  }
}

fn skip_to_expr_end(src: List(Token2), cnt: Int) {
  case src {
    [Token2(token.LeftParen, _), ..tail] -> skip_to_expr_end(tail, cnt + 1)
    [Token2(token.RightParen, _) as head, ..tail] ->
      case cnt - 1 <= 0 {
        True -> #(head, tail)
        _ -> skip_to_expr_end(tail, cnt - 1)
      }
    [_, ..tail] -> skip_to_expr_end(tail, cnt)
  }
}

fn skip_to_block_end(src: List(Token2), cnt: Int) {
  case src {
    [Token2(token.LeftBrace, _), ..tail] -> skip_to_block_end(tail, cnt + 1)
    [Token2(token.RightBrace, _) as head, ..tail] ->
      case cnt - 1 <= 0 {
        True -> #(head, tail)
        _ -> skip_to_block_end(tail, cnt - 1)
      }
    [_, ..tail] -> skip_to_block_end(tail, cnt)
  }
}

fn skip_to(src: List(Token2), token: token.Token) {
  case src {
    [Token2(hd, _) as head, ..tail] ->
      case hd == token {
        True -> #(head, src)
        _ -> skip_to(tail, token)
      }
  }
}

pub fn read(src: List(Token2)) -> #(DocSymbol2, List(Token2)) {
  let #(public, rest_a) = case src {
    [Token2(token.Pub, pos), ..rest_b] -> #(True, rest_b)
    _ -> #(False, src)
  }

  //log("### ~p] ~p",[untyped(eprev),untyped(rest_a)])
  case rest_a {
    // const
    [
      Token2(token.Const, _),
      Token2(token.Name(name), infs),
      Token2(token.Colon, infe),
      Token2(token.UpName(typename), _),
      Token2(token.Equal, _),
      ..rest
    ] -> #(DocSymbol2(DefConst(public, name), range(infs, infe)), rest)

    // const
    [
      Token2(token.Const, _),
      Token2(token.Name(name), infs),
      Token2(token.Equal, infe),
      ..rest
    ] -> #(DocSymbol2(DefConst(public, name), range(infs, infe)), rest)

    // defun 
    [
      Token2(token.Fn, _),
      Token2(token.Name(name), infs),
      Token2(token.LeftParen, _),
      ..rest
    ] -> {
      let #(Token2(_, infe), rest2) = skip_to_block_end(rest, 0)
      #(DocSymbol2(Defun(public, name), range(infs, infe)), rest2)
    }

    // TypeAlias
    // type Type = Type 
    [
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(token.Equal, infe),
      ..rest
    ] -> #(DocSymbol2(DefTypeAlias(public, name), range(infs, infe)), rest)

    // CustomType(t) {}
    [
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(token.LeftParen, infe),
      ..rest
    ] -> {
      //log("*** Custom type ~s", [name])
      let #(Token2(_, _), rest2) = skip_to_expr_end(rest, 1)
      let #(children, rest3) = read_constructors(rest2, [])
      #(
        DocSymbol2(
          ParentWithChildren(
            DocSymbol2(DefType(public, name), range(infs, infe)),
            children,
          ),
          range(infs, infe),
        ),
        rest3,
      )
    }
    // CustomType {}
    [
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(token.LeftBrace, infe),
      ..rest
    ] -> {
      //log("*** Custom type ~s", [name])
      let #(children, rest3) = read_constructors(rest, [])
      //log("CustomType children:~p", [children])
      #(
        DocSymbol2(
          ParentWithChildren(
            DocSymbol2(DefType(public, name), range(infs, infe)),
            children,
          ),
          range(infs, infe),
        ),
        rest3,
      )
    }

    // CustomType
    [
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(_, infe),
      ..rest
    ] -> #(DocSymbol2(DefType(public, name), range(infs, infe)), rest)

    // external fn
    [
      Token2(token.External, _),
      Token2(token.Fn, _),
      Token2(token.Name(name), infs),
      Token2(token.LeftParen, infe),
      ..rest
    ] -> #(DocSymbol2(DefExternalFun(public, name), range(infs, infe)), rest)

    // external type
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(token.LeftParen, infe),
      ..rest
    ] -> #(DocSymbol2(DefExternalType(public, name), range(infs, infe)), rest)
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.Name(name), infs),
      Token2(_, infe),
      ..rest
    ] -> #(DocSymbol2(DefExternalType(public, name), range(infs, infe)), rest)
    [
      Token2(token.External, _),
      Token2(token.Type, _),
      Token2(token.UpName(name), infs),
      Token2(_, infe),
      ..rest
    ] -> #(DocSymbol2(DefExternalType(public, name), range(infs, infe)), rest)

    [unknown, ..rest] ->
      //log("unknown factor: ~p", [unknown])
      read(rest)
  }
}

fn parse_(src: List(Token2), acc: List(DocSymbol2)) {
  case exception.catch_ex(fn() { read(src) }) {
    Ok(#(sym, rest)) ->
      case rest {
        [] -> list.reverse([sym, ..acc])
        _ -> parse_(rest, [sym, ..acc])
      }
    Error(_) -> list.reverse(acc)
  }
}

pub fn parse(src: List(Token2)) {
  parse_(src, [])
}
