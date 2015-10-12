package ylab.alignment.matrix

import ylab.alignment.{Horizontal, Side, Vertical}

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 0:16
 */
class AlignmentMatrix[@specialized(Char, Byte) T](maxWidth : Int, maxHeight : Int) extends Matrix(maxWidth, maxHeight) {
  private val _horizontal = new MatrixSideSequence[T](maxWidth - 1)
  private val _vertical   = new MatrixSideSequence[T](maxHeight - 1)

  override def move(i : Int) : Side => Unit = _ => {}

  @inline
  def push(c : T) : Side => Unit = {
    case Horizontal =>
      if (_width + 1 >= maxWidth) throw new IndexOutOfBoundsException("Cannot push to horizontal")
      _width += 1
      _horizontal.push(c)
    case Vertical =>
      if (_height + 1 >= maxHeight) throw new IndexOutOfBoundsException("Cannot push to vertical")
      _height += 1
      _vertical.push(c)
  }

  @inline
  def push(s : Iterable[T]) : Side => Unit = {
    case Horizontal =>
      if (_width + s.size >= maxWidth) throw new IndexOutOfBoundsException("Cannot push to horizontal")
      _width += s.size
      _horizontal.push(s)
    case Vertical =>
      if (_height + s.size >= maxHeight) throw new IndexOutOfBoundsException("Cannot push to vertical")
      _height += s.size
      _vertical.push(s)
  }

  @inline
  def pop(i : Int) : Side => Unit =
    side => {
      require(i >= 0, "Cannot pop negative number of elements")
      side match {
        case Horizontal =>
          if (_width <= i) throw new IndexOutOfBoundsException("Cannot pop from horizontal")
          _width -= i
          _horizontal.pop(i)
        case Vertical =>
          if (_height <= i) throw new IndexOutOfBoundsException("Cannot pop from vertical")
          _height -= i
          _vertical.pop(i)
      }
  }

  @inline
  def sequence : Side => Array[T] = {
    case Horizontal => _horizontal.sequence
    case Vertical => _vertical.sequence
  }

  @inline
  def horizontal : Array[T] = _horizontal.sequence

  @inline
  def vertical : Array[T] = _vertical.sequence

  def dump() : Unit = {
    (0 until _height).foreach {
      case row =>
        (0 until _width).foreach {
          case col =>
            print(s"${apply(row, col)}\t")
        }
        println()
    }
  }
}
