@external(erlang, "file", "read_file")
pub fn file_get_bytes(path: String) -> Result(String, reason)

pub fn get_bytes(path: String) {
  file_get_bytes(path)
}
