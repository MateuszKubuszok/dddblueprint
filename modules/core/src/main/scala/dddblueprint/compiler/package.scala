package dddblueprint

import cats.mtl.MonadState

package object compiler extends syntax {

  type SnapshotState[StateIO[_]] = MonadState[StateIO, output.Snapshot]
  object SnapshotState {
    @inline def apply[StateIO[_]](implicit StateIO: SnapshotState[StateIO]): SnapshotState[StateIO] = StateIO
  }
}
