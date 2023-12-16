import otp/erlang
import gleam/option.{type Option, None, Some}
import gleam/list
import lib/log.{log}

pub type Char =
  Int

type NextCodePointResult

@external(erlang, "erlang_ffi", "unicode_next_codepoint")
fn unicode_next_codepoint(s: String) -> Option(#(Int, String))

@external(erlang, "unicode", "characters_to_binary")
fn unicode_characters_to_binary(ls: List(Char)) -> String

fn string_to_list_(src: String, acc: List(Char)) {
  case unicode_next_codepoint(src) {
    None -> list.reverse(acc)
    Some(#(charcode, rest)) -> string_to_list_(rest, [charcode, ..acc])
  }
}

pub fn utf8_string_to_list(src: String) -> List(Char) {
  string_to_list_(src, [])
}

pub fn string_to_list(src: String) -> List(Char) {
  utf8_string_to_list(src)
}

pub fn get_first_char(src: String) -> Char {
  case unicode_next_codepoint(src) {
    None -> 0
    Some(#(charcode, _)) -> charcode
  }
}

pub fn list_to_string(src: List(Char)) -> String {
  unicode_characters_to_binary(src)
}
