package game

import munit.*

class HelloWorldSuite extends FunSuite:
  test("hello world test") {
    assert(1 + 1 == 2)
  }

  test("TickTimer") {
    val timer = TickTimer.start(1000)

    assert(TickTimer.isActive(timer))

  }
end HelloWorldSuite
