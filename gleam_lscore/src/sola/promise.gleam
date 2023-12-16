import otp/timer
import lib/log.{log}
import gleam/string

pub type PromiseException

//pub external type Promise
pub type Promise(a) =
  fn() -> Result(a, PromiseException)

@external(erlang, "erlang_ffi", "make_promise")
pub fn make_promise(f: fn() -> res) -> Promise(res)

pub fn test() {
  log("PROMISE ---------------------------", [])

  let p3 =
    make_promise(fn() -> String {
      let p1 =
        make_promise(fn() -> String {
          timer.sleep(1000 * 3)
          "Hello World"
        })

      let p2 =
        make_promise(fn() -> String {
          timer.sleep(1000 * 5)
          "Uho! iiotoko!"
        })
      let assert Ok(r1) = p1()
      let assert Ok(r2) = p2()
      //let Error(_) = p2()
      string.concat([r1, r2])
    })
  //string.concat([r1])
  let val = p3()
  log("result = ~p ==================", [val])
}
