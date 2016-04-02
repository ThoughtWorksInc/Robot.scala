package com.thoughtworks

import utest._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object RobotTests extends TestSuite {

  object MyState {

    def xxx = 1
  }

  override def tests = this {

//    object R extends Robot(MyState)

//    R.nextState
//    R.state.xxx

  }
}
