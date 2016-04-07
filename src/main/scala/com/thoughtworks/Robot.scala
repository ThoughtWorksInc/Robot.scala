package com.thoughtworks

import java.io._
import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}
import java.security.MessageDigest
import java.util.{Arrays, Date}

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.{FileUtils, IOUtils}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.{Symbols, Trees}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.reflect.api.Position
import scala.reflect.internal.util.SourceFile
import scala.reflect.macros
import scala.reflect.macros.blackbox
import scala.util.{Failure, Success}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
abstract class Robot[AutoImports] private[Robot](val initialState: AutoImports, persistencyPosition: Robot.PersistencyPosition, private var autoImportsType: Type)
  extends Robot.StateMachine(initialState) {
  Singleton =>

  def this(initialState: AutoImports)(implicit persistencyPosition: Robot.PersistencyPosition, tag: WeakTypeTag[AutoImports]) = {
    this(initialState, persistencyPosition, tag.tpe)
  }

  override final def state_=(newState: Any): Unit = synchronized {
    if (currentState != newState) {
      import Q._
      Robot.SourceFilePatcher.edit(persistencyPosition, showCode(q"$newState"))
      currentState = newState
      autoImportsType = currentMirror.classSymbol(newState.getClass).toType
    }
  }

  final def eval(code: String): Unit = {
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

  private[thoughtworks] final class EditingSourceFile(file: PersistencyFile) {
    var md5sum: Array[Byte] = file.md5sum
    val sizeChanges: Array[Int] = Array.ofDim[Int](file.numberOfRobots)
  }

  private[thoughtworks] class SourceFilePatcher {

    private[thoughtworks] val editingSourceFiles = new mutable.WeakHashMap[String, EditingSourceFile]

    final def edit(position: PersistencyPosition, newContent: String): Unit = {
      val editingSourceFile = editingSourceFiles.synchronized {
        editingSourceFiles.getOrElseUpdate(position.file.name.intern(), {
          new EditingSourceFile(position.file)
        })
      }
      editingSourceFile.synchronized {

        val currentStart = position.initialStart + editingSourceFile.sizeChanges.view(0, position.index).sum
        val currentEnd = position.initialEnd + editingSourceFile.sizeChanges(position.index)

        if (Arrays.equals(DigestUtils.md5(FileUtils.readFileToByteArray(new File(position.file.name))), editingSourceFile.md5sum)) {
          val reader = new InputStreamReader(new FileInputStream(position.file.name), io.Codec.UTF8.charSet)
          val (before, after) = try {
            val before = Array.ofDim[Char](currentStart)
            IOUtils.readFully(reader, before)
            IOUtils.skip(reader, currentEnd - currentStart)
            val after = IOUtils.toCharArray(reader)
            (before, after)
          } finally {
            reader.close()
          }

          val writer = new OutputStreamWriter(new FileOutputStream(position.file.name), io.Codec.UTF8.charSet)
          try {
            IOUtils.write(before, writer)
            IOUtils.write(newContent, writer)
            IOUtils.write(after, writer)
          } finally {
            writer.close()
          }
          editingSourceFile.md5sum = DigestUtils.md5(FileUtils.readFileToByteArray(new File(position.file.name)))
        } else {
          throw new IllegalStateException(s"Can't patch ${position.file.name} because some other programs changes on file after the robot loaded.")
        }
      }
    }

  }

  private[thoughtworks] object SourceFilePatcher extends SourceFilePatcher

  final case class PersistencyFile(name: String, numberOfRobots: Int, md5sum: Array[Byte])

  final case class PersistencyPosition(file: PersistencyFile, index: Int, initialStart: Int, initialEnd: Int)

  object PersistencyPosition {

    import scala.language.experimental.macros

    implicit final def currentPersistencyPosition: PersistencyPosition = macro Macros.currentPersistencyPosition

  }

  private[Robot] object Macros {

    private type Index = Int

    private val positionMapCache = new mutable.WeakHashMap[macros.Universe#CompilationUnit, Array[PatchPoint]]

    private final case class PatchPoint(constructor: Trees#Tree, position: reflect.api.Position)

    final def currentPersistencyPosition(c: blackbox.Context): c.Tree = {
      import c.universe._

      val positionMap = positionMapCache.synchronized {
        val unit = c.enclosingUnit
        positionMapCache.getOrElseUpdate(unit, {
          val array = {
            val arrayBuilder = Array.newBuilder[PatchPoint]

            val traverser = new Traverser {
              override def traverse(tree: Tree): Unit = {
                tree match {
                  case md@ModuleDef(_, _, Template(List(q"$_($stateTree)"), _, List(constructor))) =>
                    arrayBuilder += PatchPoint(constructor, stateTree.pos)
                  case _ =>
                    super.traverse(tree)
                }

              }
            }
            traverser(unit.body)
            arrayBuilder.result()
          }
          Arrays.sort(array, Ordering.by[PatchPoint, Int](_.position.start))
          array
        })
      }

      val (index, objectPosition) = c.enclosingMethod match {
        case EmptyTree =>
          val index = positionMap.indexWhere(_.constructor.symbol == c.internal.enclosingOwner)
          index -> positionMap(index).position
        case currentRobotConstructor =>
          val index = positionMap.indexWhere(_.constructor == currentRobotConstructor)
          if (index == -1) {
            c.error(c.enclosingPosition, "A Robot instance must be an object, not a class.")
            return q"???"
          }
          index -> positionMap(index).position
      }

      if (objectPosition.start == objectPosition.end) {
        c.error(c.enclosingPosition, "Robot.scala requires ranged position. Please add setting `scalacOptions += \"-Yrangepos\"` to your build.sbt")
      }

      val md5sum = DigestUtils.md5(FileUtils.readFileToByteArray(objectPosition.source.file.file))
      val pp = PersistencyPosition(
        PersistencyFile(objectPosition.source.path, positionMap.size, md5sum),
        index,
        objectPosition.start,
        objectPosition.end
      )
      import Q._
      q"$pp"
    }

  }

}