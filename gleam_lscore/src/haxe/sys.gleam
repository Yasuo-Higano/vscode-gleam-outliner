@external(erlang, "file", "get_cwd")
pub fn file_getcwd() -> Result(String, reason)

pub fn get_cwd() {
  file_getcwd()
}
