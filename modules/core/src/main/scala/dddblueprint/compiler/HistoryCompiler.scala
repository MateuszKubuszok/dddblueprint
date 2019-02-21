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
        lastSnapshot <- migration.compile[IO]
      } yield oldBlueprint.lens(_.versions).modify(_ :+ lastSnapshot)
    }
}

object HistoryCompiler {

  @inline def apply[F[_]](implicit historyCompiler: HistoryCompiler[F]): HistoryCompiler[F] = historyCompiler
}
