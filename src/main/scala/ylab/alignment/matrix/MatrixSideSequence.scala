package ylab.alignment.matrix

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 0:09
 */
class MatrixSideSequence[@specialized(Char, Byte) T](val maxSize : Int) {
  private [this] val _sequence = Array.fill[T](maxSize)(null.asInstanceOf[T])
  private [this] var _realSize = 0

  @inline
  def push(s : Iterable[T]) : Unit = s.foreach(push)

  @inline
  def push(c : T) : Unit = {
    _sequence(_realSize) = c
    _realSize += 1
  }

  @inline
  def pop(i : Int = 1) : Unit =
    if (_realSize >= i) {
      _realSize -= i
    }
    else throw new IndexOutOfBoundsException(s"Cannot pop $i elements from side")

  @inline
  def size : Int = _realSize

  @inline
  def sequence : Array[T] = _sequence
}