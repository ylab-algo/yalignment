package ylab.alignment.logic.simple

import ylab.alignment.TracebackResult
import ylab.alignment.logic.{MatrixType, PrimaryMatrix}

/**
 * User: pavel
 * Date: 12.10.15
 * Time: 14:21
 */
class LocalAlignment[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int,
                                                 substitution : (T, T) => Int,
                                                 gap : Int) extends SimpleAlignment[T](maxWidth, maxHeight, substitution, gap) {
  private [this] var _maxScore = Int.MinValue
  private [this] var _colOfMax = -1
  private [this] var _rowOfMax = -1

  override protected def value(row : Int, col : Int) : MatrixType => Int = {
    case PrimaryMatrix =>
      if (row == 0 || col == 0) 0
      else {
        val currentScore = math.max(math.max(math.max(_primaryMatrix(row - 1, col), _primaryMatrix(row, col - 1)) + gap,
                                                      _primaryMatrix(row - 1, col - 1) + score(row - 1, col - 1)), 0)
        if (currentScore > _maxScore) {
          _maxScore = currentScore
          _rowOfMax = row
          _colOfMax = col
        }
        currentScore
      }
    case _ => throw new IllegalArgumentException("Only Primary matrix type can be used in simple alignment")
  }

  @inline
  override protected def prepare() : TracebackResult[T] = {
    val tr = TracebackResult[T](score = _maxScore)
    tr.row = _rowOfMax
    tr.col = _colOfMax

    tr
  }

  @inline
  override protected def cycleCondition(ts : TracebackResult[T]) : Boolean =
    ts.row != 0 && ts.col != 0 && _primaryMatrix(ts.row, ts.col) != 0
}