@external(erlang, "file", "read_file")
pub fn read_file(path: String) -> Result(String, reason)

@external(erlang, "file", "write_file")
pub fn write_file(path: String, data: String) -> Result(ok, reason)

@external(erlang, "file", "get_cwd")
pub fn get_cwd() -> Result(String, reason)

@external(erlang, "file", "list_dir")
pub fn list_dir(dir: String) -> Result(List(String), reason)
