package dddblueprint
package compiler

import cats.Monad
import cats.implicits._
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached final class HistoryCompiler[IO[_]: Monad: FixedMigrationCompiler] {

  def apply(history: input.History): IO[output.Blueprint] =
    history.migrations.foldLeft(output.Blueprint().pure[IO]) { (previousVersion, migration) =>
      for {
        oldBlueprint <- previousVersion
        lastSnapshot <- migration.compile[IO](oldBlueprint.versions.lastOption)
      } yield oldBlueprint.lens(_.versions).modify(_ :+ lastSnapshot)
    }
}

object HistoryCompiler {

  @inline def apply[IO[_]](implicit historyCompiler: HistoryCompiler[IO]): HistoryCompiler[IO] = historyCompiler
}
