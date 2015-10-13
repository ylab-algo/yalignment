package ylab.alignment.logic

import ylab.alignment.TracebackResult
import ylab.alignment.matrix.AlignmentMatrix

/**
 * User: pavel
 * Date: 10.10.15
 * Time: 22:47
 */
abstract class Traceback[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int) {
  protected val _primaryMatrix = new AlignmentMatrix[T](maxWidth, maxHeight)

  @inline
  protected def prepare() : TracebackResult[T]

  @inline
  protected def cycleCondition(ts : TracebackResult[T]) : Boolean
  @inline
  protected def substitutionCondition(ts : TracebackResult[T]) : Boolean
  @inline
  protected def verticalCondition(ts : TracebackResult[T]) : Boolean
  @inline
  protected def horizontalCondition(ts : TracebackResult[T]) : Boolean

  final def traceback() : TracebackResult[T] = {
    val horizontal = _primaryMatrix.horizontal
    val vertical = _primaryMatrix.vertical

    val tr = prepare()

    while (cycleCondition(tr)) {
      val c = if (tr.row > 0) vertical(tr.row - 1) else null.asInstanceOf[T]
      val d = if (tr.col > 0) horizontal(tr.col - 1) else null.asInstanceOf[T]

      if (substitutionCondition(tr)) {
        tr.vertical = Option(c) :: tr.vertical
        tr.horizontal = Option(d) :: tr.horizontal
        tr.row -= 1; tr.col -= 1
      }
      else if (verticalCondition(tr)) {
        tr.vertical = Option(c) :: tr.vertical
        tr.horizontal = None :: tr.horizontal
        tr.row -= 1
      }
      else if (horizontalCondition(tr)) {
        tr.vertical = None :: tr.vertical
        tr.horizontal = Option(d) :: tr.horizontal
        tr.col -= 1
      }
      else assert(assertion = false)
    }

    tr
  }

  @inline
  def width : Int = _primaryMatrix.width

  @inline
  def height : Int = _primaryMatrix.height
}