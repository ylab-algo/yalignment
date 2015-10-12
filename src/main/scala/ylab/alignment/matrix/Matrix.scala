package ylab.alignment.matrix

import ylab.alignment.matrix.Matrix.MatrixCoordinateAny
import ylab.alignment.{Horizontal, Side, Vertical}

/**
 * User: pavel
 * Date: 10.10.15
 * Time: 20:16
 */
class Matrix(val maxWidth : Int, val maxHeight : Int) {
  private [this] val _matrix = Array.fill[Int](maxWidth * maxHeight)(0)

  protected var _width = 1
  protected var _height = 1

  @inline
  def width : Int = _width

  @inline
  def height : Int = _height

  @inline
  def move(i : Int) : Side => Unit = {
    case Horizontal => _width += i
    case Vertical => _height += i
  }

  @inline
  final def apply(idx : Int) : Int = _matrix(idx)

  @inline
  final def apply(row : Int, col : Int) : Int = apply(decodeCoordiates(row, col))

  @inline
  final def update(idx : Int, value : Int) : Unit = _matrix(idx) = value

  @inline
  final def update(row : Int, col : Int, value : Int) : Unit = update(decodeCoordiates(row, col), value)

  @inline
  final def update(row : MatrixCoordinateAny, col : Int, value : Int => Int) : Unit = {
    val end = decodeCoordiates(_height - 1, col)
    ((col to end by maxWidth) zip (0 until _height)).foreach {
      case (idx, i) =>
        val v = value(i)
        update(idx, v)
    }
  }

  @inline
  final def update(row : Int, col : MatrixCoordinateAny, value : Int => Int) : Unit = {
    val start = rowStart(row)
    (start until start + _width).foreach {
      case idx =>
        val v = value(idx - start)
        update(idx, v)
    }
  }

  @inline
  final def max() : (Int, (Int, Int)) = {
    (0 until _height).flatMap {
      case row =>
        val start = rowStart(row)
        (start until start + _width).map {
          case idx =>
            (_matrix(idx), (row, idx - start))
        }
    }.maxBy(_._1)
  }

  @inline
  final def rowMax(row : Int) : (Int, Int) = {
    val start = rowStart(row)
    (start until start + _width).map { case idx => (apply(idx), idx - start) }.maxBy(_._1)
  }

  @inline
  final def colMax(col : Int) : (Int, Int) = {
    val end = decodeCoordiates(_height - 1, col)
    ((col to end by maxWidth) zip (0 until _height)).map {
      case (idx, row) =>
        (apply(idx), row)
    }.maxBy(_._1)
  }

  @inline
  private [this] def decodeCoordiates(row : Int, col : Int) = rowStart(row) + col

  @inline
  private [this] def rowStart(row : Int) : Int = maxWidth * row
}

object Matrix {
  sealed trait MatrixCoordinateAny
  case object :*: extends MatrixCoordinateAny
}