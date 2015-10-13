package ylab.alignment.logic.simple

import ylab.alignment.{Vertical, Horizontal, TracebackResult}
import ylab.alignment.logic.{MatrixType, PrimaryMatrix}

/**
 * User: pavel
 * Date: 12.10.15
 * Time: 15:11
 */
class SemiglobalAlignment[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int,
                                                      substitution : (T, T) => Int,
                                                      gap : Int) extends SimpleAlignment[T](maxWidth, maxHeight, substitution, gap) {
  override protected def value(row : Int, col : Int) : MatrixType => Int = {
    case PrimaryMatrix =>
      if (row == 0 || col == 0) 0
      else {
        math.max(math.max(_primaryMatrix(row - 1, col), _primaryMatrix(row, col - 1)) + gap,
                 _primaryMatrix(row - 1, col - 1) + score(row - 1, col - 1))
      }
    case _ => throw new IllegalArgumentException("Only Primary matrix type can be used in simple alignment")
  }

  @inline
  override protected def prepare() : TracebackResult[T] = {
    val (rs, col) = _primaryMatrix.rowMax(height - 1)
    val (cs, row) = _primaryMatrix.colMax(width - 1)

    val tr = TracebackResult[T](score = math.max(rs, cs))

    if (rs >= cs) {
      tr.row = height - 1
      tr.col = col

      val seq = _primaryMatrix.sequenceCopy(Horizontal)
      var i = width - 1
      while (i > col) {
        tr.horizontal = Some(seq(i - 1)) :: tr.horizontal
        tr.vertical = None :: tr.vertical
        i -= 1
      }
    }
    else {
      tr.row = row
      tr.col = width - 1

      val seq = _primaryMatrix.sequenceCopy(Vertical)

      var i = height - 1
      while (i > row) {
        tr.horizontal = None :: tr.horizontal
        tr.vertical = Some(seq(i - 1)) :: tr.vertical
        i -= 1
      }
    }

    tr
  }
}
