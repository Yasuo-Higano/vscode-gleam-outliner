external fn system_time() -> Float = "erlang" "system_time"

pub fn timestamp() -> Float {
    system_time() /. 1000000000.0
}
