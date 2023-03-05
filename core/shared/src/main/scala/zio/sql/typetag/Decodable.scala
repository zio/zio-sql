package zio.sql.typetag

import java.sql.ResultSet
import zio.sql.select.DecodingError

trait Decodable[+A] {
  def decode(column: Int, resultSet: ResultSet): Either[DecodingError, A]
}
