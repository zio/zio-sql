package zio.sql

import zio._
import zio.stream.ZStream
import zio.schema.Schema
import zio.sql.update._
import zio.sql.select._
import zio.sql.insert._
import zio.sql.delete._

trait TransactionSyntaxModule { self: Jdbc =>
  implicit final class ReadSyntax[A](self: Read[A]) {
    def run: ZStream[SqlTransaction, Exception, A] =
      ZStream.serviceWithStream(_.read(self))
  }

  implicit final class DeleteSyntax(self: Delete[_]) {
    def run: ZIO[SqlTransaction, Exception, Int] =
      ZIO.serviceWithZIO(_.delete(self))
  }

  implicit final class BatchDeleteSyntax[A: Schema](self: List[Delete[_]]) {
    def run: ZIO[SqlTransaction, Exception, Int] =
      ZIO.serviceWithZIO(_.delete(self))
  }

  implicit final class InsertSyntax[A: Schema](self: Insert[_, A]) {
    def run: ZIO[SqlTransaction, Exception, Int] =
      ZIO.serviceWithZIO(_.insert(self))
  }

  implicit final class UpdatedSyntax(self: Update[_]) {
    def run: ZIO[SqlTransaction, Exception, Int] =
      ZIO.serviceWithZIO(_.update(self))
  }

  implicit final class BatchUpdatedSyntax(self: List[Update[_]]) {
    def run: ZIO[SqlTransaction, Exception, Int] =
      ZIO.serviceWithZIO(_.update(self))
  }
}
