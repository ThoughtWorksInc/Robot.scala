package com.thoughtworks

import java.io._

import org.apache.commons.io.FileUtils

import scala.reflect.api.Symbols
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
abstract class Robot[AutoImports] private[Robot](currentState: Any, sourceFile: File, autoImportsType: Type)
  extends Robot.StateMachine(currentState) {

  def this(currentState: AutoImports)(implicit currentSource: Robot.PersistencyPosition, tag: WeakTypeTag[AutoImports]) = {
    this(currentState, currentSource.get, tag.tpe)
  }

  private def symbol = currentMirror.moduleSymbol(getClass)

  private def packageName = symbol.owner.fullName

  private def name = symbol.name.toString

  override final def state_=(newState: Any): Unit = {
    Robot.persistState(newState, sourceFile, symbol.owner, name)
  }

  final def main(arguments: Array[String]): Unit = {
    arguments match {
      case Array(code) =>
        import scala.tools.reflect.ToolBox
        val toolBox = currentMirror.mkToolBox()

        val autoImports = state
        val tree =
          q"""
          val $$robotAutoImports = ${reify(autoImports).tree}.asInstanceOf[$autoImportsType]
          import $$robotAutoImports._
          ${toolBox.parse(code)}
        """
        state = toolBox.eval(tree)
        sys.exit()
      case _ =>
        sys.error("Expect one argument.")
    }
  }

}

object Robot {

  sealed abstract class StateMachine private[Robot](private[Robot] var currentState: Any) {

    final def state: Any = currentState

    def state_=(newState: Any): Unit

  }

  final class Remote private[Robot](initialState: Any, sourceFile: File, packageSymbol: Symbols#Symbol, name: String) extends StateMachine(initialState) {
    override def state_=(newState: Any): Unit = {
      persistState(newState, sourceFile, packageSymbol, name)
    }
  }

  def madeRobot(state: Any, sourceFile: File, packageName: String, name: String): Remote = {
    val packageSymbol = currentMirror.staticPackage(packageName)
    persistState(state, sourceFile, packageSymbol, name: String)
    new Remote(state, sourceFile, packageSymbol, name)
  }

  private def persistState(state: Any, sourceFile: File, packageName: String, name: String): Unit = {
    persistState(state, sourceFile, currentMirror.staticPackage(packageName), name: String)
  }

  private def persistState(state: Any, sourceFile: File, packageSymbol: Symbols#Symbol, name: String): Unit = {
    import Q._
    val content =
      q"""
        package ${MacroBundle(reflect.runtime.universe).fullyQualifiedSymbolTree(packageSymbol).asInstanceOf[RefTree]} {
          object ${newTermName(name)} extends _root_.com.thoughtworks.Robot($state)
        }
      """
    FileUtils.write(sourceFile, showCode(content), io.Codec.UTF8.charSet)
  }

  final class PersistencyPosition(val get: File)

  object PersistencyPosition {

    import scala.language.experimental.macros

    implicit final def currentSource: PersistencyPosition = macro Macros.currentSource

  }

  private[Robot] object Macros {

    import scala.reflect.macros.whitebox.Context

    final def currentSource(c: Context): c.Tree = {
      import c.universe._
//
//      c.info(c.enclosingPosition, show(c.reifyEnclosingRuntimeClass), true)
//      c.info(c.enclosingPosition, show(c.reifyEnclosingRuntimeClass.pos), true)

      val source = c.enclosingPosition.source
      c.enclosingPosition.start
      c.enclosingPosition.end
      val et = c.macroApplication
      
      c.info(c.enclosingPosition, show(et), true)



      val p = c.internal.enclosingOwner.pos

//      c.info(p, "c.i.e.i", true)
//      c.info(c.macroApplication.asInstanceOf[scala.reflect.internal.Trees#Tree], "ma.p", true)

      q"""new _root_.com.thoughtworks.Robot.PersistencyPosition(new _root_.java.io.File(${c.enclosingPosition.source.path}))"""

      // TODO: other information
      //  ???
    }

  }

}