package zio.sql

import zio._
import zio.test._
import zio.test.Assertion._

import HelloWorld._

object HelloWorld {

  def sayHello: ZIO[Console, Throwable, Unit] =
    Console.printLine("Hello, World!")
}

object HelloWorldSpec extends DefaultRunnableSpec {

  def spec = suite("HelloWorldSpec")(
    test("sayHello correctly displays output") {
      for {
        _      <- sayHello
        output <- TestConsole.output
      } yield assert(output)(equalTo(Vector("Hello, World!\n")))
    }
  )
}
