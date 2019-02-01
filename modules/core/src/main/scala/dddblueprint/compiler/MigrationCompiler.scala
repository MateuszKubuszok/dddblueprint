package dddblueprint
package compiler

import cats.implicits._
import cats.mtl.implicits._
import cats.Monad
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, ValidatedNel }
import dddblueprint.compiler.MigrationCompiler.Intermediate

class MigrationCompiler[F[_]: Monad: SnapshotState: SchemaErrorHandle](actionCompiler: ActionCompiler[F],
                                                                       validateTransition: ValidateTransition[F]) {

  def apply(migration: input.Migration): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      _ <- combineMigrations(migration.actions, oldVersion)
      newVersion <- SnapshotState[F].get
      _ <- validateTransition(oldVersion, newVersion)
    } yield ()

  def combineMigrations(actions: List[input.Action], initialState: output.Snapshot): F[Unit] =
    Monad[F].tailRecM[Intermediate, Unit](Intermediate(actions, initialState)) {
      case Intermediate(action :: toProcess, lastValid, currentState) =>
        (for {
          _ <- actionCompiler(action)
          newSnapshot <- SnapshotState[F].get
          newState = (currentState, newSnapshot.validNel[SchemaError]).mapN((_, _) => ())
        } yield Intermediate(toProcess, newSnapshot, newState).asLeft[Unit]).handle[NonEmptyList[SchemaError]] {
          errors =>
            // snapshot compilation as a whole would fail but this way we can gather more errors at once
            Intermediate(toProcess, lastValid, (currentState, errors.invalid[Unit]).mapN((_, _) => ())).asLeft[Unit]
        }

      case Intermediate(Nil, _, Valid(_)) =>
        ().asRight[Intermediate].pure[F]

      case Intermediate(Nil, _, Invalid(errors)) =>
        errors.raise[F, Either[Intermediate, Unit]]
    }
}

object MigrationCompiler {

  private final case class Intermediate(actions:      List[input.Action],
                                        lastValid:    output.Snapshot,
                                        currentState: ValidatedNel[SchemaError, Unit] = ().validNel[SchemaError])
}
