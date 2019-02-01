package dddblueprint
package compiler

import cats.Monad
import cats.implicits._
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached class HistoryCompiler[F[_]: Monad: SnapshotState](migrationCompiler: MigrationCompiler[F]) {

  def apply(history: input.History): F[output.Blueprint] =
    history.migrations.foldLeft(output.Blueprint().pure[F]) { (previousVersion, migration) =>
      for {
        oldBlueprint <- previousVersion
        _ <- migrationCompiler(migration)
        lastSnapshot <- SnapshotState[F].get
      } yield oldBlueprint.lens(_.versions).modify(_ :+ lastSnapshot)
    }
}
