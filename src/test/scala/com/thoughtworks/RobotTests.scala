package com.thoughtworks

import utest._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object RobotTests extends TestSuite {

  sealed trait Door

  final case class OpenDoor(lives: Int) extends Door {
    def close = if (lives > 0) ClosedDoor(lives - 1) else BrokenDoor
  }

  final case class ClosedDoor(lives: Int) extends Door {
    def open = if (lives > 0) OpenDoor(lives - 1) else BrokenDoor
  }

  case object BrokenDoor extends Door

  override def tests = this {

    "object R extends Robot" - {
      object R extends Robot[Door](_root_.com.thoughtworks.RobotTests.ClosedDoor(5))

      R.eval("open")
      assert(R.state == OpenDoor(4))
      R.eval("close")
      assert(R.state == ClosedDoor(3))
      R.eval("open")
      assert(R.state == OpenDoor(2))
      R.eval("close")
      assert(R.state == ClosedDoor(1))
      R.eval("open")
      assert(R.state == OpenDoor(0))
      R.eval("close")
      assert(R.state == BrokenDoor)

      R.state = ClosedDoor(5)
    }

    "new Robot {}" - {

      val r = new Robot[Door](_root_.com.thoughtworks.RobotTests.OpenDoor(0)) {}

      r.eval("close")
      assert(r.state == BrokenDoor)

      r.state = OpenDoor(0)
    }

    "Robot(...)" - {

      val r = Robot[Door](_root_.com.thoughtworks.RobotTests.OpenDoor(0))

      r.eval("close")
      assert(r.state == BrokenDoor)

      r.state = OpenDoor(0)
    }

  }


}
