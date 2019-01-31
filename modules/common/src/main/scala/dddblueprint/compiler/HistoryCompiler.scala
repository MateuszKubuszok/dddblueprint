package dddblueprint
package compiler

import cats.Monad
import cats.implicits._
import monocle.macros.syntax.lens._

class HistoryCompiler[F[_]: Monad: SnapshotState](migrationCompiler: MigrationCompiler[F]) {

  def apply(history: schema.History): F[validated.Blueprint] =
    history.migrations.foldLeft(validated.Blueprint().pure[F]) { (previousVersion, migration) =>
      for {
        oldBlueprint <- previousVersion
        _ <- migrationCompiler(migration)
        lastSnapshot <- SnapshotState[F].get
      } yield oldBlueprint.lens(_.versions).modify(_ :+ lastSnapshot)
    }
}
