package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import zio._
import zio.blocking.{effectBlocking, Blocking}

object TestContainer {
  
  def container[C <: SingleContainer[_] : Tag](c: C): ZLayer[Blocking, Nothing, Has[C]] =
    ZManaged.make {
      effectBlocking {
        c.start()
        c
      }.orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer

}
