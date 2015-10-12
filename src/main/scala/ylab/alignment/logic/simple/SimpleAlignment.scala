package ylab.alignment.logic.simple

import ylab.alignment.TracebackResult
import ylab.alignment.logic.{Alignment, MatrixType, PrimaryMatrix}

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 11:05
 */
abstract class SimpleAlignment[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int,
                                                           substitution : (T, T) => Int,
                                                           gap : Int) extends Alignment[T](maxWidth, maxHeight, substitution) {

  override protected def value(row : Int, col : Int) : MatrixType => Int = {
    case PrimaryMatrix =>
      if (row == 0) gap * col
      else if (col == 0) gap * row
      else {
        math.max(math.max(_primaryMatrix(row - 1, col), _primaryMatrix(row, col - 1)) + gap,
                 _primaryMatrix(row - 1, col - 1) + score(row - 1, col - 1))
      }
    case _ => throw new IllegalArgumentException("Only Primary matrix type can be used in simple alignment")
  }

  @inline
  override protected def cycleCondition(ts : TracebackResult[T]) : Boolean =
    ts.row != 0 || ts.col != 0

  @inline
  override protected def substitutionCondition(ts : TracebackResult[T]) : Boolean =
    ts.row != 0 && ts.col != 0 && _primaryMatrix(ts.row, ts.col) == _primaryMatrix(ts.row - 1, ts.col - 1) + score(ts.row - 1, ts.col - 1)

  @inline
  override protected def verticalCondition(ts : TracebackResult[T]) : Boolean =
    ts.row != 0 && (ts.col == 0 || _primaryMatrix(ts.row, ts.col) == _primaryMatrix(ts.row - 1, ts.col) + gap)

  @inline
  override protected def horizontalCondition(ts : TracebackResult[T]) : Boolean =
    ts.col != 0 && (ts.row == 0 || _primaryMatrix(ts.row, ts.col) == _primaryMatrix(ts.row, ts.col - 1) + gap)
}
