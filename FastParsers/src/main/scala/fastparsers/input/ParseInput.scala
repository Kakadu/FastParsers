package fastparsers.input

import scala.reflect.macros.whitebox.Context

/**
 * General interface to work on an fastparsers.input in the macro world.
 */
trait ParseInput {
  val c: Context
  type Elem
  type Input

  import c.universe._

  def inputType: c.Tree
  def inputElemType: c.Tree
  def inputWindowType: c.Tree = tq"fastparsers.input.InputWindow.InputWindow[$inputElemType]"

  def initInput(startpos: c.Tree, then: c.Tree): c.Tree

  def currentInput: c.Tree

  def advance: c.Tree

  def setpos(pos: c.Tree): c.Tree

  def mark(code: c.Tree => c.Tree): c.Tree

  def isEOI: c.Tree

  def isNEOI: c.Tree

  def pos: c.Tree

  def slice(begin: c.Tree, end: c.Tree): c.Tree

  def inputsize: c.Tree

  def getPositionned(offset: c.Tree): c.Tree = q"NoPosition"

  def getInputWindow(start: c.Tree, end: c.Tree): c.Tree = q"new fastparsers.input.InputWindow.InputWindow[$inputElemType](input, $start, $end)"
}