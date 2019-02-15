package dddblueprint

import cats.mtl.MonadState

package object compiler extends syntax {

  type SnapshotState[F[_]] = MonadState[F, output.Snapshot]
  object SnapshotState { @inline def apply[F[_]](implicit F: SnapshotState[F]): SnapshotState[F] = F }
}
