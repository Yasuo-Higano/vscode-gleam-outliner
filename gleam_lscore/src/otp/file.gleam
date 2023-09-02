pub external fn read_file(path: String) -> Result(String, reason) =
  "file" "read_file"

pub external fn write_file(path: String, data: String) -> Result(ok, reason) =
  "file" "write_file"

pub external fn get_cwd() -> Result(String, reason) =
  "file" "get_cwd"

pub external fn list_dir(dir: String) -> Result(List(String), reason) =
  "file" "list_dir"
