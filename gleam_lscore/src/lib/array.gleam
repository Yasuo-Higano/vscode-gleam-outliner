import otp/array as otp_array
import gleam.{List}
import lib/log.{log}

pub type Array(t) =
  otp_array.Array(t)

pub fn new() -> Array(t) {
  otp_array.new()
}

pub fn new_fixed(size: Int) -> Array(t) {
  otp_array.new_fixed(size)
}

pub fn size(array: Array(t)) -> Int {
  otp_array.size(array)
}

pub fn get(array: Array(t), index: Int) -> t {
  otp_array.get(index, array)
}

pub fn set(array: Array(t), index: Int, value: t) -> Array(t) {
  otp_array.set(index, value, array)
}

pub fn from_list(list: List(t)) -> Array(t) {
  otp_array.from_list(list)
}

pub fn to_list(array: Array(t)) -> List(t) {
  otp_array.to_list(array)
}

pub fn test() {
  let a: Array(String) = new()
  let a2 = set(a, 0, "Uho")
  let a3 = set(a2, 4, "IIOTOKO")
  log("array size = ~p", [size(a3)])
  log("array->list = ~p", [to_list(a3)])
}
