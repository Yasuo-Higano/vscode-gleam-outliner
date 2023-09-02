external fn file_getcwd() -> Result(String,reason) = "file" "get_cwd"

pub fn get_cwd() {
    file_getcwd()
}