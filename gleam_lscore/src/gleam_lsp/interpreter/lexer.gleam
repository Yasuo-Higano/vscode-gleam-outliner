import gleam/option.{type Option, None, Some}
import gleam/list
import gleam/bit_string
import gleam/string
import gleam/io
import lib/log.{log}
import otp/erlang
import gleam_lsp/interpreter/token.{type Token}
import lib/untyped.{untyped}
import gleam_lsp/interpreter/itype.{type SrcInf, type Token2, SrcInf, Token2}
import gleam/result

fn srcpos(line: Int, col: Int) -> SrcInf {
  SrcInf(line: line, col: col, inf: None)
}

pub fn str_to_ukeyword(word: String) -> Option(Token) {
  // Alphabetical keywords:
  case word {
    "List" -> Some(token.List)
    "True" -> Some(token.True)
    "False" -> Some(token.False)
    "Quote" -> Some(token.Quote)
    _ -> None
  }
}

pub fn str_to_keyword(word: String) -> Option(Token) {
  // Alphabetical keywords:
  case word {
    "as" -> Some(token.As)
    "assert" -> Some(token.Assert)
    "case" -> Some(token.Case)
    "const" -> Some(token.Const)
    "external" -> Some(token.External)
    "@external" -> Some(token.External)
    "fn" -> Some(token.Fn)
    "if" -> Some(token.If)
    "import" -> Some(token.Import)
    "let" -> Some(token.Let)
    "opaque" -> Some(token.Opaque)
    "pub" -> Some(token.Pub)
    "set" -> Some(token.Set)
    "todo" -> Some(token.Todo)
    "try" -> Some(token.Try)
    "type" -> Some(token.Type)
    "when" -> Some(token.When)
    "cond" -> Some(token.Cond)
    "elseif" -> Some(token.ElseIf)
    "else" -> Some(token.Else)
    _ -> None
  }
}

fn lex_comment_block(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"*/":utf8, rest:bits>> -> {
      log("comment block = ~s", [acc])
      #(acc, rest, srcpos(pos.line, pos.col + 1))
    }
    _ -> lex_string_(lex_comment_block, str, acc, pos)
  }
}

fn lex_line(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line + 1, 0))

    <<chr, rest:bits>> ->
      lex_line(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))
    <<chr>> -> lex_line(<<>>, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))

    <<>> -> #(acc, <<>>, srcpos(pos.line, pos.col + 1))
  }
}

fn lex_string_(f, str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> f(rest, acc, srcpos(pos.line + 1, 0))

    <<"\\":utf8, chr, rest:bits>> -> {
      let escaped_chr = case chr {
        0x22 -> "\""
        0x5c -> "\\"
        //0x65 -> "\e"
        0x66 -> "\f"
        0x6e -> "\n"
        0x72 -> "\r"
        0x74 -> "\t"
      }
      f(rest, <<acc:bits, escaped_chr:utf8>>, srcpos(pos.line, pos.col + 2))
    }
    <<chr, rest:bits>> ->
      f(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))

    _ -> {
      log("EOF ~s", [str])
      #(<<>>, acc, pos)
    }
  }
}

fn lex_string(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\"":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line, pos.col + 1))
    _ -> lex_string_(lex_string, str, acc, pos)
  }
}

fn lex_singlequoted_string(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"'":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line, pos.col + 1))
    _ -> lex_string_(lex_singlequoted_string, str, acc, pos)
  }
}

fn is_number(chr: Int) {
  0x30 <= chr && chr <= 0x39
}

fn is_lower_alphabet(chr: Int) {
  0x61 <= chr && chr <= 0x7a
}

fn is_upper_alphabet(chr: Int) {
  0x41 <= chr && chr <= 0x5a
}

fn is_alphabet(chr: Int) {
  is_lower_alphabet(chr) || is_upper_alphabet(chr)
}

fn is_registered_symbol(chr: Int) {
  case chr {
    0x21 -> True
    // !
    0x23 -> True
    // #
    0x24 -> True
    // $
    0x25 -> True
    // %
    0x26 -> True
    // &
    0x2a -> True
    // *
    0x2b -> True
    // +
    0x2d -> True
    // -
    0x2e -> True
    // .
    0x2f -> True
    // /
    0x3a -> True
    // :
    0x3c -> True
    // <
    0x3d -> True
    // =
    0x3e -> True
    // >
    0x3f -> True
    // ?
    0x40 -> True
    // @
    0x5c -> True
    // \
    0x5e -> True
    // ^
    0x5f -> True
    // _
    0x7c -> True
    // |
    0x7e -> True
    // ~
    _ -> False
  }
}

type CharType {
  Number(chr: Int)
  LAlphabet(chr: Int)
  UAlphabet(chr: Int)
  Symbol(chr: Int)
}

fn classify_char(chr: Int) -> CharType {
  case is_number(chr) {
    True -> Number(chr)
    False ->
      case is_alphabet(chr) {
        True ->
          case is_lower_alphabet(chr) {
            True -> LAlphabet(chr)
            False -> UAlphabet(chr)
          }
        False -> Symbol(chr)
      }
  }
}

fn lex_name(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line + 1, 0))
    <<chr, rest:bits>> ->
      case classify_char(chr) {
        Symbol(95) | Symbol(64) -> {
          // _ @
          lex_name(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))
        }

        Symbol(ch) -> {
          //log("symbol ~p", [ch])
          #(acc, str, pos)
        }

        _ -> lex_name(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))
      }
    _ -> #(acc, str, pos)
  }
}

fn lex_decimal(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line + 1, 0))
    <<chr, rest:bits>> ->
      case is_number(chr) {
        True ->
          lex_decimal(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1))
        False -> #(acc, str, pos)
      }
    _ -> #(acc, str, pos)
  }
}

fn lex_hexadecimal(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> #(acc, rest, srcpos(pos.line + 1, 0))
    <<chr, rest:bits>> ->
      case is_number(chr) || 0x61 <= chr && chr <= 0x66 {
        True ->
          lex_hexadecimal(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1),
          )
        False -> #(acc, str, pos)
      }
    _ -> #(acc, str, pos)
  }
}

fn lex_number(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<"0x":utf8, rest:bits>> -> lex_hexadecimal(rest, <<>>, pos)

    _ -> lex_decimal(str, <<>>, pos)
  }
}

fn lex_float(str: BitArray, acc: BitArray, pos: SrcInf) {
  case str {
    <<chr, rest:bits>> ->
      case is_number(chr) || 0x61 <= chr && chr <= 0x66 {
        True ->
          lex_hexadecimal(rest, <<acc:bits, chr>>, srcpos(pos.line, pos.col + 1),
          )
        False -> #(acc, str, pos)
      }
    _ -> #(acc, str, pos)
  }
}

fn parse_atom(src: BitArray, acc: List(Token2), pos: SrcInf) {
  //log("parse_atom ~p", [src])
  case src {
    <<"_":utf8, rest:bits>> ->
      parse_next(lex_name(src, <<>>, pos), src, acc, pos, fn(str, pos) {
        Some(Token2(token.DiscardName(str), pos))
      })

    <<"@":utf8, rest:bits>> ->
      parse_next(lex_name(src, <<>>, pos), src, acc, pos, fn(str, pos) {
        Some(Token2(token.DecoratorName(str), pos))
      })

    <<chr, rest:bits>> ->
      case classify_char(chr) {
        Number(ch) -> {
          let #(number, urest, new_pos) = lex_number(src, <<>>, pos)
          case urest {
            <<".":utf8, urest2:bits>> ->
              parse_next(
                lex_float(
                  urest2,
                  <<number:bits, ".":utf8>>,
                  srcpos(pos.line, pos.col + 1),
                ),
                src,
                acc,
                pos,
                fn(str, pos) { Some(Token2(token.Float(str), pos)) },
              )
            _ ->
              parse_next(#(number, urest, new_pos), src, acc, pos, fn(str, pos) {
                Some(Token2(token.Int(str), pos))
              })
          }
        }
        LAlphabet(ch) ->
          parse_next(lex_name(src, <<>>, pos), src, acc, pos, fn(str, pos) {
            let t = case str_to_keyword(str) {
              Some(keyword_token) -> keyword_token
              _ -> token.Name(str)
            }
            Some(Token2(t, pos))
          })
        UAlphabet(ch) ->
          parse_next(lex_name(src, <<>>, pos), src, acc, pos, fn(str, pos) {
            let t = case str_to_ukeyword(str) {
              Some(keyword_token) -> keyword_token
              _ -> token.UpName(str)
            }
            Some(Token2(t, pos))
          })

        // 謎の記号から始まる文字列
        _ -> {
          log("*** parse_atom ~p", [src])
          parse_next(lex_name(src, <<>>, pos), src, acc, pos, fn(str, pos) {
            Some(Token2(token.UnknownName(str), pos))
          })
        }
      }
  }
}

fn parse_next(
  tokenized,
  src: BitArray,
  acc: List(Token2),
  pos: SrcInf,
  f: fn(String, SrcInf) -> Option(Token2),
) {
  let #(bitstr, urest, new_pos) = tokenized
  let assert Ok(str) = bit_string.to_string(bitstr)
  let new_acc = case f(str, pos) {
    Some(token) -> [token, ..acc]
    None -> acc
  }
  parse_loop(urest, new_acc, new_pos)
}

pub fn parse_loop(src: BitArray, acc: List(Token2), pos: SrcInf) {
  // log("parse_loop ~p ~s:", #(
  //   pos,
  //   string.slice(
  //     bit_string.to_string(src)
  //     |> result.unwrap(""),
  //     0,
  //     32,
  //   ),
  // ))

  case src {
    <<"\n\r":utf8, rest:bits>>
    | <<"\r\n":utf8, rest:bits>>
    | <<"\r":utf8, rest:bits>>
    | <<"\n":utf8, rest:bits>> -> parse_loop(rest, acc, srcpos(pos.line + 1, 0))

    <<"////":utf8, rest:bits>> ->
      parse_next(
        lex_line(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.CommentModule(str), pos) },
        fn(str, pos) { None },
      )

    <<"///":utf8, rest:bits>> ->
      parse_next(lex_line(rest, <<>>, pos), src, acc, pos, fn(str, pos) { None })

    <<"//":utf8, rest:bits>> ->
      parse_next(
        lex_line(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.CommentNormal(str), pos) },
        fn(str, pos) { None },
      )

    // とりあえずコメント扱いにしてスキップする
    <<"@external(":utf8, rest:bits>> ->
      parse_next(
        lex_line(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.CommentNormal(str), pos) },
        fn(str, pos) { None },
      )
    <<"/***":utf8, rest:bits>> ->
      parse_next(
        lex_comment_block(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.BlockCommentModule(str), pos) },
        fn(str, pos) { None },
      )

    <<"/**":utf8, rest:bits>> ->
      parse_next(
        lex_comment_block(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.BlockCommentDoc(str), pos) },
        fn(str, pos) { None },
      )

    <<"/*":utf8, rest:bits>> ->
      parse_next(
        lex_comment_block(rest, <<>>, pos),
        src,
        acc,
        pos,
        //fn(str, pos) { Token2(token.BlockCommentNormal(str), pos) },
        fn(str, pos) { None },
      )

    <<"++":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.PlusPlus, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"--":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.MinusMinus, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<":=":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.ColonEqual, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )

    <<"/.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.SlashDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"/":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Slash, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"==":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.EqualEqual, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"=":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Equal, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"+.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.PlusDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"+":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Plus, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"*.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.StarDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"*":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Star, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"%":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Percent, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"||":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.VbarVbar, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"|>":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Pipe, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"|":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Vbar, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"&&":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.AmperAmper, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )

    <<"-.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.MinusDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"->":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.RArrow, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"-":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Minus, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"!=":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.NotEqual, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"!":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Not, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )

    <<"(":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LeftParen, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    <<")":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.RightParen, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    <<"[":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LeftSquare, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    <<"]":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.RightSquare, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    <<"{":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LeftBrace, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    <<"}":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.RightBrace, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<":":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Colon, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"<<":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LtLt, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"<.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LessDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<"<=.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LessEqualDot, pos), ..acc],
        srcpos(pos.line, pos.col + 3),
      )
    <<"<=":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.LessEqual, pos), ..acc],
        srcpos(pos.line, pos.col + 3),
      )
    <<"<":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Less, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<">>":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.GtGt, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<">.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.GreaterDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<">=.":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.GreaterEqualDot, pos), ..acc],
        srcpos(pos.line, pos.col + 3),
      )
    <<">=":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.GreaterEqual, pos), ..acc],
        srcpos(pos.line, pos.col + 3),
      )
    <<">":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Greater, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"..":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.DotDot, pos), ..acc],
        srcpos(pos.line, pos.col + 2),
      )
    <<".":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Dot, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<",":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Comma, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"#":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Hash, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"'":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Quote, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"`":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.QuasiQuote, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )
    // skip white space 
    <<" ":utf8, rest:bits>> | <<"\t":utf8, rest:bits>> | <<";":utf8, rest:bits>> ->
      parse_loop(rest, acc, srcpos(pos.line, pos.col + 1))

    //    <<"\n\r":utf8, rest:bits>>
    //    | <<"\r\n":utf8, rest:bits>>
    //    | <<"\r":utf8, rest:bits>>
    //    | <<"\n":utf8, rest:bits>> -> parse_loop(rest, acc, srcpos(pos.line + 1, 0))
    <<"~\\":utf8, rest:bits>> ->
      parse_loop(
        rest,
        [Token2(token.Lambda, pos), ..acc],
        srcpos(pos.line, pos.col + 1),
      )

    <<"~":utf8, rest:bits>> ->
      parse_next(lex_name(rest, <<>>, pos), src, acc, pos, fn(str, pos) {
        Some(Token2(token.Sigil(string.concat(["~", str])), pos))
      })

    <<"\"":utf8, rest:bits>> ->
      parse_next(lex_string(rest, <<>>, pos), src, acc, pos, fn(str, pos) {
        Some(Token2(token.String(str), pos))
      })

    <<>> ->
      list.reverse([Token2(token.EOF, pos), Token2(token.EOF, pos), ..acc])

    //    <<a, rest:bits>> -> {
    //      case a <= 31 || a >= 127 {
    //        True -> parse_loop(rest, acc, srcpos(pos.line + 1, 0))
    //        _ -> parse_atom(src, acc, srcpos(pos.line, pos.col + 1))
    //      }
    //    }
    <<a, rest:bits>> -> {
      case is_alphabet(a) {
        True -> parse_atom(src, acc, srcpos(pos.line, pos.col + 1))
        _ -> parse_loop(rest, acc, srcpos(pos.line, pos.col + 1))
      }
    }
  }
}

pub fn parse(str: String) {
  //log("parse", [])
  let bstr = bit_string.from_string(str)
  //log("parse_loop", [])
  let res = parse_loop(bstr, [], srcpos(0, 0))
  //log("parse done [ ~p ]", [res])
  res
}

pub fn test() {
  let token_list =
    parse(
      "\"Hello World!\" =
    \"\\t\\\"How are you?\" ==
//// module comment
// normal comment
let a = 0x123
let b=0.234
let c = -0.1234
let d = 0x123abc
let e = \"string data\"

pub type TestType {
    TestType(msg: String)
}

pub fn testfun(a:Int, b:Float) -> String {

}


/// doc coment",
    )
  log("LEXER ----------------------------------------------------", [])
  log("~p", [token_list])
}
