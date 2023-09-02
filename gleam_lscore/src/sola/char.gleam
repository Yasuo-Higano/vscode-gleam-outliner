import otp/erlang.{list_to_tuple}
import gleam/option.{None, Option, Some}
import gleam/list
import lib/log.{log}

pub type Char =
  Int

external type NextCodePointResult

external fn unicode_next_codepoint(String) -> Option(#(Int, String)) =
  "erlang_ffi" "unicode_next_codepoint"

external fn unicode_characters_to_binary(List(Char)) -> String =
  "unicode" "characters_to_binary"

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
