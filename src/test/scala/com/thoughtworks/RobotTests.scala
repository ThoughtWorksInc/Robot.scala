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


    //    object R extends Robot(MyState)

    //    R.nextState
    //    R.state.xxx

  }


}
