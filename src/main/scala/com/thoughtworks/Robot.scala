package com.thoughtworks

import java.io._

import org.apache.commons.io.FileUtils

import scala.reflect.api.Symbols
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
abstract class Robot private(val sourceFile: File) {

  val currentState: Any

  def this()(implicit currentSource: Robot.PersistencyPosition) = this(currentSource.get)

  def symbol = currentMirror.moduleSymbol(getClass)

  def packageName = symbol.owner.fullName

  def name = symbol.name.toString

  final def persistState(newState: Any): Unit = {
    Robot.persistState(newState, sourceFile, symbol.owner, name)
  }

  final def main(arguments: Array[String]): Unit = {
    arguments match {
      case Array(code) =>
        import scala.tools.reflect.ToolBox
        val toolBox = currentMirror.mkToolBox()
        val newState = toolBox.eval(q"""
          import $symbol.currentState._
          ${toolBox.parse(code)}
        """)
        persistState(newState.asInstanceOf[Any])
      case _ =>
        sys.error("""Usage: sbt run 'yourMethodInYouState("your parameter")'""")
    }
  }

}

object Robot {
  def persistState(state: Any, sourceFile: File, packageName: String, name: String): Unit = {
    persistState(state, sourceFile, currentMirror.staticPackage(packageName), name: String)
  }

  def persistState(state: Any, sourceFile: File, packageSymbol: Symbols#Symbol, name: String): Unit = {
    import Q._
    val content =
      q"""
        package ${MacroBundle(reflect.runtime.universe).fullyQualifiedSymbolTree(packageSymbol).asInstanceOf[RefTree]} {
          object ${newTermName(name)} extends _root_.com.thoughtworks.Robot {
            val currentState = $state
          }
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
      q"""new _root_.com.thoughtworks.Robot.PersistencyPosition(new _root_.java.io.File(${c.enclosingPosition.source.path}))"""

      // TODO: other information
    }

  }

}