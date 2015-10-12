package ylab.alignment.logic.simple

import ylab.alignment.TracebackResult

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 11:39
 */
class GlobalAlignment[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int,
                                                  substitution : (T, T) => Int,
                                                  gap : Int) extends SimpleAlignment[T](maxWidth, maxHeight, substitution, gap) {
  @inline
  override protected def prepare() : TracebackResult[T] = {
    val tr = TracebackResult[T](score = _primaryMatrix(height - 1, width - 1))
    tr.col = width - 1
    tr.row = height - 1
    tr
  }
}
