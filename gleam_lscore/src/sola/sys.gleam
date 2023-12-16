@external(erlang, "erlang", "system_time")
pub fn system_time() -> Float

pub fn timestamp() -> Float {
  system_time() /. 1_000_000_000.0
}
