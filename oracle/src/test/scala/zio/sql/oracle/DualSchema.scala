package zio.sql.oracle

import zio.schema.{ DeriveSchema, Schema }
import zio.sql.table._

trait DualSchema {
  object Dual {
    case class Dual(dummy: String)

    implicit val dummySchema: Schema.CaseClass1[String, Dual] = DeriveSchema.gen[Dual]
    val dual                                                  = Table.defineTable[Dual]
    val dummy                                                 = dual.columns
  }
}
