package ylab.alignment.logic

import ylab.alignment.matrix.Matrix
import ylab.alignment.matrix.Matrix.:*:
import ylab.alignment.{Horizontal, Side, Vertical}

/**
 * User: pavel
 * Date: 10.10.15
 * Time: 22:46
 */
abstract class Alignment[@specialized(Char, Byte) T](maxWidth : Int,
                                                     maxHeight : Int,
                                                     substitution : (T, T) => Int) extends Traceback[T](maxWidth, maxHeight) {

  @inline
  protected def value(row : Int, col : Int) : MatrixType => Int

  @inline
  protected def matricies : Map[MatrixType, Matrix] = Map.empty[MatrixType, Matrix]

  @inline
  def push(s : Iterable[T])(side : Side) : Unit = {
    val (oWidth, oHeight) = (width, height)

    _primaryMatrix.push(s)(side)
    matricies.values.foreach(_.move(s.size)(side))

    side match {
      case Horizontal =>
        (oWidth to width).foreach(updateHorizontal)
      case Vertical =>
        (oHeight to height).foreach(updateVertical)
    }
  }

  @inline
  def push(c : T)(side : Side) : Unit = {
    _primaryMatrix.push(c)(side)
    matricies.values.foreach(_.move(1)(side))

    side match {
      case Horizontal => updateHorizontal(width)
      case Vertical => updateVertical(height)
    }
  }

  @inline
  def pop(i : Int = 1)(side : Side) : Unit = {
    _primaryMatrix.pop(i)(side)
    matricies.values.foreach(_.move(-i)(side))
  }

  @inline
  protected def score(i : Int, j : Int) : Int =
    substitution(_primaryMatrix.sequence(Vertical)(i), _primaryMatrix.sequence(Horizontal)(j))

  @inline
  private def updateHorizontal(w : Int) : Unit = {
    matricies.foreach { case (t, m) => m(:*:, w - 1) = (row : Int) => value(row, w - 1)(t) }
    _primaryMatrix(:*:, w - 1) = (row : Int) => value(row, w - 1)(PrimaryMatrix)
  }

  @inline
  private def updateVertical(h : Int) : Unit = {
    matricies.foreach { case (t, m) => m(h - 1, :*:) = (col : Int) => value(h - 1, col)(t) }
    _primaryMatrix(h - 1, :*:) = (col : Int) => value(h - 1, col)(PrimaryMatrix)
  }

  def dump() : Unit = _primaryMatrix.dump()
}
