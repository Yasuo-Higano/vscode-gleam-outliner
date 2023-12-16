import otp/erlang.{type Unit}

pub type Second =
  Float

pub type Millisecond =
  Int

@external(erlang, "timer", "sleep")
pub fn timer_sleep(time: Int) -> Unit

//pub fn sleep(time:Second) -> Unit {
//    timer_sleep( trunc(time *. 1000.0) )
//}

pub fn sleep(time: Millisecond) -> Unit {
  timer_sleep(time)
}
