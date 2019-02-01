package dddblueprint

import cats.data.NonEmptyList
import cats.mtl.{ ApplicativeHandle, FunctorRaise, MonadState }

package object compiler {

  type SnapshotState[F[_]] = MonadState[F, output.Snapshot]
  object SnapshotState { @inline def apply[F[_]](implicit F: SnapshotState[F]): SnapshotState[F] = F }

  type SchemaErrorHandle[F[_]] = ApplicativeHandle[F, NonEmptyList[SchemaError]]
  object SchemaErrorHandle { @inline def apply[F[_]](implicit F: SchemaErrorHandle[F]): SchemaErrorHandle[F] = F }

  type SchemaErrorRaise[F[_]] = FunctorRaise[F, NonEmptyList[SchemaError]]
  object SchemaErrorRaise { @inline def apply[F[_]](implicit F: SchemaErrorRaise[F]): SchemaErrorRaise[F] = F }
}
