//// comment

pub type Token {
  Sigil(name: String)
  Name(name: String)
  UpName(name: String)
  DiscardName(name: String)
  DecoratorName(name: String)
  UnknownName(name: String)
  Int(value: String)
  Float(value: String)
  String(value: String)
  // Groupings
  LeftParen
  // (
  RightParen
  // )
  LeftSquare
  // [
  RightSquare
  // )
  LeftBrace
  // {
  RightBrace
  // }
  // Int Operators
  Plus
  Minus
  Star
  Slash
  Less
  Greater
  LessEqual
  GreaterEqual
  Percent
  // Float Operators
  PlusDot
  // '+.'
  MinusDot
  // '-.'
  StarDot
  // '*.'
  SlashDot
  // '/.'
  LessDot
  // '<.'
  GreaterDot
  // '>.'
  LessEqualDot
  // '<=.'
  GreaterEqualDot
  // '>=.'
  //
  PlusPlus
  // ++
  MinusMinus
  // --
  ColonEqual
  // :=
  // Other Punctuation
  Colon
  Comma
  Hash
  // '#'
  Equal
  EqualEqual
  // '=='
  NotEqual
  Not
  // '!='
  Vbar
  // '|'
  VbarVbar
  // '||'
  AmperAmper
  // '&&'
  LtLt
  // '<<'
  GtGt
  // '>>'
  Pipe
  // '|>'
  Dot
  // '.'
  RArrow
  // '->'
  DotDot
  // '..'
  EndOfFile
  // Extra
  CommentNormal(value: String)
  // comment
  CommentDoc(value: String)
  /// comment
  CommentModule(value: String)
  BlockCommentNormal(value: String)
  // /* comment
  BlockCommentDoc(value: String)
  // /** comment
  BlockCommentModule(value: String)
  // /*** comment
  EmptyLine
  // Keywords (alphabetically):
  As
  Assert
  Case
  Const
  External
  Fn
  If
  Import
  Let
  Opaque
  Pub
  Set
  Todo
  Try
  Type
  Lambda
  List
  True
  False
  When
  Cond
  ElseIf
  Else
  Quote
  QuasiQuote

  EOF
  ERROR
}
