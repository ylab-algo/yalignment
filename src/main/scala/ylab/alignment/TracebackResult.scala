package ylab.alignment

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 10:57
 */
case class TracebackResult[@specialized(Char, Byte) T](score : Int,
                                                       var horizontal : List[Option[T]] = List.empty[Option[T]],
                                                       var vertical : List[Option[T]] = List.empty[Option[T]]) {
  var row = 0
  var col = 0
}
