package zio.sql.select

import zio.sql.typetag._

sealed trait DecodingError extends Exception {
  def message: String
}

object DecodingError {
  final case class UnexpectedNull(column: Int)                       extends DecodingError {
    def message = s"Expected column with index ${column} to be non-null"
  }
  final case class UnexpectedType(expected: TypeTag[_], actual: Int) extends DecodingError {
    def message = s"Expected type ${expected} but found ${actual}"
  }
  final case class MissingColumn(column: Int)                        extends DecodingError {
    def message = s"The column with index ${column} does not exist"
  }
  case object Closed                                                  extends DecodingError {
    def message = s"The ResultSet has been closed, so decoding is impossible"
  }
}