package dddblueprint

import cats.mtl.{ FunctorRaise, MonadState }

package object compiler {

  type SnapshotState[F[_]] = MonadState[F, output.Snapshot]
  object SnapshotState { def apply[F[_]: SnapshotState]: SnapshotState[F] = implicitly[SnapshotState[F]] }

  type SchemaErrorRaise[F[_]] = FunctorRaise[F, SchemaError]
  object SchemaErrorRaise { def apply[F[_]: SchemaErrorRaise]: SchemaErrorRaise[F] = implicitly[SchemaErrorRaise[F]] }
}
