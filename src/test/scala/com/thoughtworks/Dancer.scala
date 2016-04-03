package com.thoughtworks

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
trait Dancer {
  def dance: Dancer
}

object Dancer {

  case object qop extends Dancer {
    override def dance = dop
  }

  object dop extends Dancer {
    override def dance = dob
  }

  object dob extends Dancer {
    override def dance = qob
  }

  object qob extends Dancer {
    override def dance = qop
  }

}