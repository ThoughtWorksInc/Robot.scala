package com.thoughtworks

import java.io._

import scala.language.experimental.macros
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.{FileUtils, IOUtils}

import scala.collection.mutable
import scala.reflect.api.{Symbols, Trees}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.reflect.macros.{Universe, blackbox}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
abstract class Robot[State] private[Robot](private[Robot] var currentState: State, writeState: State => Unit) {

  def this(initialState: State)(implicit persistencyPosition: Robot.PersistencyPosition) = {
    this(initialState, { newState: State =>
      import Q._
      Robot.SourceFilePatcher.edit(persistencyPosition, showCode(q"$newState"))
    })
  }

  final def state: State = currentState

  final def state_=(newState: State): Unit = synchronized {
    if (currentState != newState) {
      import Q._
      writeState(newState)
      currentState = newState
    }
  }

  final def eval(code: String): State = {
    import scala.tools.reflect.ToolBox
    val toolBox = currentMirror.mkToolBox()

    val autoImports = state
    val autoImportsType = currentMirror.classSymbol(autoImports.getClass).toType
    val tree =
      q"""
        val $$robotState = ${reify(autoImports).tree}.asInstanceOf[$autoImportsType]
        import $$robotState._
        ${toolBox.parse(code)}
      """
    toolBox.eval(tree).asInstanceOf[State]
  }

  final def main(arguments: Array[String]): Unit = {
    arguments match {
      case Array(code) =>
        eval(code)
      case _ =>
        sys.error("Expect one argument.")
    }
  }

}

object Robot {

  /**
    * Returns a newly created [[Robot]] at `sourceFile` with `packageName`.`objectName`.
    */
  def at[State](state: State, sourceFile: File, packageName: String, objectName: String): Robot[State] = {
    val packageSymbol = currentMirror.staticPackage(packageName)
    persistState(state, sourceFile, packageSymbol, objectName: String)
    new Robot[State](state, persistState(_, sourceFile, packageSymbol, objectName)) {}
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

  private[thoughtworks] final class EditingSourceFile(name: String, var md5sum: Array[Byte]) {
    val sizeChanges = mutable.Map.empty[PersistencyPosition, Int]
  }

  private[thoughtworks] class SourceFilePatcher {

    private[thoughtworks] val editingSourceFiles = new mutable.WeakHashMap[String, EditingSourceFile]

    final def edit(currentPosition: PersistencyPosition, newContent: String): Unit = {
      val editingSourceFile = editingSourceFiles.synchronized {
        editingSourceFiles.getOrElseUpdate(currentPosition.fileName.intern(), {
          new EditingSourceFile(currentPosition.fileName, currentPosition.md5sum)
        })
      }
      editingSourceFile.synchronized {

        val beforeOffset = (for {
          (position, offset) <- editingSourceFile.sizeChanges.view
          if position.initialStart < currentPosition.initialStart
        } yield offset).sum

        val currentOffset = editingSourceFile.sizeChanges.getOrElseUpdate(currentPosition, 0)

        val currentStart = currentPosition.initialStart + beforeOffset
        val currentEnd = currentPosition.initialEnd + beforeOffset + currentOffset

        if (java.util.Arrays.equals(DigestUtils.md5(FileUtils.readFileToByteArray(new File(currentPosition.fileName))), editingSourceFile.md5sum)) {
          val reader = new InputStreamReader(new FileInputStream(currentPosition.fileName), io.Codec.UTF8.charSet)
          val (before, after) = try {
            val before = Array.ofDim[Char](currentStart)
            IOUtils.readFully(reader, before)
            IOUtils.skip(reader, currentEnd - currentStart)
            val after = IOUtils.toCharArray(reader)
            (before, after)
          } finally {
            reader.close()
          }

          val writer = new OutputStreamWriter(new FileOutputStream(currentPosition.fileName), io.Codec.UTF8.charSet)
          try {
            IOUtils.write(before, writer)
            IOUtils.write(newContent, writer)
            IOUtils.write(after, writer)
          } finally {
            writer.close()
          }
          editingSourceFile.sizeChanges(currentPosition) += newContent.length - (currentEnd - currentStart)
          editingSourceFile.md5sum = DigestUtils.md5(FileUtils.readFileToByteArray(new File(currentPosition.fileName)))
        } else {
          throw new IllegalStateException(s"Can't patch ${currentPosition.fileName} because some other programs changes on file after the robot loaded.")
        }
      }
    }

  }

  private[thoughtworks] object SourceFilePatcher extends SourceFilePatcher

  final case class PersistencyPosition(fileName: String, md5sum: Array[Byte], initialStart: Int, initialEnd: Int)

  object PersistencyPosition {

    implicit def here: PersistencyPosition = macro Macros.here

  }

  def apply[State](state: State): Robot[State] = macro Macros.apply

  private[Robot] object Macros {

    def apply(c: blackbox.Context)(state: c.Tree): c.Tree = {
      import c.universe._
      val md5sum = DigestUtils.md5(FileUtils.readFileToByteArray(state.pos.source.file.file))
      val pp = new PersistencyPosition(
        state.pos.source.path,
        md5sum,
        state.pos.start,
        state.pos.end
      )
      val q"""$_[$t]($_)""" = c.macroApplication
      import Q._
      q"""new _root_.com.thoughtworks.Robot[$t]($state)($pp) {}"""
    }

    private val positionMapCache = new mutable.WeakHashMap[Universe#CompilationUnit, Array[ConstructorPatchPoint]]

    private final case class ConstructorPatchPoint(constructor: Trees#Tree, position: reflect.api.Position)

    def here(c: blackbox.Context): c.Tree = {
      import c.universe._

      val positionMap = positionMapCache.synchronized {
        val unit = c.enclosingUnit
        positionMapCache.getOrElseUpdate(unit, {
          val constructorPatchPositions = {
            val constrctorPatchPositionBuilder = Array.newBuilder[ConstructorPatchPoint]

            val traverser = new Traverser {
              override def traverse(tree: Tree): Unit = {
                tree match {
                  case Template(List(q"$robot($stateTree)"), _, constructor :: _) =>
                    constrctorPatchPositionBuilder += ConstructorPatchPoint(constructor, stateTree.pos)
                  case _ =>
                }
                super.traverse(tree)
              }
            }
            traverser(unit.body)
            constrctorPatchPositionBuilder.result()
          }
          constructorPatchPositions
        })
      }

      val index = positionMap.indexWhere(_.constructor.symbol == c.internal.enclosingOwner)

      if (index != -1) {
        val objectPosition = positionMap(index).position
        if (!objectPosition.isRange) {
          c.error(c.enclosingPosition, "Robot.scala requires ranged position. Please add setting `scalacOptions += \"-Yrangepos\"` to your build.sbt")
        }

        val md5sum = DigestUtils.md5(FileUtils.readFileToByteArray(objectPosition.source.file.file))
        val pp = new PersistencyPosition(
          objectPosition.source.path,
          md5sum,
          objectPosition.start,
          objectPosition.end
        )
        import Q._
        q"$pp"
      } else {
        q"???"
      }
    }

  }

}