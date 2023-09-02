import otp/erlang.{Unit, Atom, Pid, trunc}

pub type Second = Float
pub type Millisecond = Int

external fn timer_sleep(time:Int) -> Unit = "timer" "sleep"

//pub fn sleep(time:Second) -> Unit {
//    timer_sleep( trunc(time *. 1000.0) )
//}

pub fn sleep(time:Millisecond) -> Unit {
    timer_sleep( time )
}

