external fn file_get_bytes(path:String) -> Result(String,reason) = "file" "read_file"

pub fn get_bytes(path:String) {
    file_get_bytes(path)
}