package zio.sql.oracle

import zio.sql.Sql

trait OracleSqlModule extends Sql { self =>

  object OracleFunctionDef {
    val Sind = FunctionDef[Double, Double](FunctionName("sind"))
  }

}
