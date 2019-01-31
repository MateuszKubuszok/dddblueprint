package dddblueprint
package compiler

import cats.implicits._
import cats.Monad

class MigrationCompiler[F[_]: Monad: SnapshotState](actionCompiler: ActionCompiler[F],
                                                    validateTransition: ValidateTransition[F]) {

  def apply(migration: schema.Migration): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      _ <- migration.actions.foldLeft(SnapshotState[F].modify(_.bumpVersion)) { (state, action) =>
        state.flatMap(_ => actionCompiler(action))
      }
      newVersion <- SnapshotState[F].get
      _ <- validateTransition(oldVersion, newVersion)
    } yield ()
}
