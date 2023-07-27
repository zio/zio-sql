package zio

import zio.sql.expr.Expr

package object sql {

  type Lens[F, S, A] = Expr[Features.Source[F, S], S, A]

  type Prism[F, S, A] = Unit

  type Traversal[S, A] = Unit
}
