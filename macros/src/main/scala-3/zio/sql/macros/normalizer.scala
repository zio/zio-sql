package zio.sql.macros

import scala.language.experimental.macros

sealed trait Normalizer[In] {
  type Out
}

// TODO transparent inline
object Normalizer {

//   final case class Instance[In, Out2]() extends Normalizer[In] {
//     override type Out = Out2
//   }

  implicit def createNormalizer[In]: Normalizer[In] = {
    new Normalizer[In] {}
  }

}
