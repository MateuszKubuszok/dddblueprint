package dddblueprint
package compiler

import alleycats.Zero
import cats.implicits._
import cats.mtl.implicits._
import cats.{ ~>, FlatMap, Monad }
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, ValidatedNel }
import dddblueprint.compiler.MigrationCompiler.Intermediate
import io.scalaland.pulp.Cached

sealed trait FixedMigrationCompiler[IO[_]] {

  def apply(migration: input.Migration, previousSnapshot: Option[output.Snapshot]): IO[output.Snapshot]
}

object FixedMigrationCompiler {

  @inline def apply[IO[_]](implicit fixedMigrationCompiler: FixedMigrationCompiler[IO]): FixedMigrationCompiler[IO] =
    fixedMigrationCompiler
}

// scalastyle:off no.whitespace.after.left.bracket
// format: off
@Cached final class MigrationCompiler[
  StateIO[_]: Monad
            : SnapshotState
            : SchemaErrorHandle
            : ActionCompiler
            : ValidateTransition
            : ApplicableDiffResolver
            : ManualDiffResolver,
  IO[_]     : FlatMap
](implicit runState: Zero[output.Snapshot] => (StateIO ~> IO))
    extends FixedMigrationCompiler[IO] {
// format: on
// scalastyle:on no.whitespace.after.left.bracket

  def apply(migration: input.Migration, previousSnapshot: Option[output.Snapshot]): IO[output.Snapshot] = {
    val startingFrom: Zero[output.Snapshot] = Zero(previousSnapshot.getOrElse(output.Snapshot()))
    runState(startingFrom).apply[output.Snapshot](
      for {
        oldVersion <- SnapshotState[StateIO].get
        _ <- combineMigrations(migration.actions, oldVersion)
        newVersion <- SnapshotState[StateIO].get
        _ <- oldVersion.validateTransition[StateIO](newVersion)
        validatedVersion <- SnapshotState[StateIO].get
        _ <- validatedVersion.resolveApplicableDiffs[StateIO](migration)
        _ <- validatedVersion.resolveManualDiffs[StateIO](migration)
        result <- SnapshotState[StateIO].get
      } yield result
    )
  }

  def combineMigrations(actions: List[input.Action], initialState: output.Snapshot): StateIO[Unit] =
    Monad[StateIO].tailRecM[Intermediate, Unit](Intermediate(actions, initialState)) {
      case Intermediate(action :: toProcess, lastValid, currentState) =>
        (for {
          _ <- ActionCompiler[StateIO].apply(action)
          newSnapshot <- SnapshotState[StateIO].get
          newState = (currentState, newSnapshot.validNel[SchemaError]).mapN((_, _) => ())
        } yield Intermediate(toProcess, newSnapshot, newState).asLeft[Unit]).handle[NonEmptyList[SchemaError]] {
          errors =>
            // snapshot compilation as a whole would fail but this way we can gather more errors at once
            Intermediate(toProcess, lastValid, (currentState, errors.invalid[Unit]).mapN(_ |+| _)).asLeft[Unit]
        }

      case Intermediate(Nil, _, Valid(_)) =>
        ().asRight[Intermediate].pure[StateIO]

      case Intermediate(Nil, _, Invalid(errors)) =>
        errors.raise[StateIO, Either[Intermediate, Unit]]
    }
}

object MigrationCompiler {

  @inline def apply[StateIO[_], IO[_]](
    implicit migrationCompiler: MigrationCompiler[StateIO, IO]
  ): MigrationCompiler[StateIO, IO] = migrationCompiler

  private final case class Intermediate(
    actions:      List[input.Action],
    lastValid:    output.Snapshot,
    currentState: ValidatedNel[SchemaError, Unit] = ().validNel[SchemaError]
  )
}
