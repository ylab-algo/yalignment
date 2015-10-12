package ylab.alignment.logic

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 0:49
 */
sealed trait MatrixType
case object PrimaryMatrix extends MatrixType
case object SubstitutionMatrix extends MatrixType
case object VerticalMatrix extends MatrixType
case object HorizontalMatrix extends MatrixType
