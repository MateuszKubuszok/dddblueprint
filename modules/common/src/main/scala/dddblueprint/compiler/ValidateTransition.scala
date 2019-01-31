package dddblueprint
package compiler

import cats.Monad
import cats.implicits._

class ValidateTransition[F[_]: Monad] {

  // TODO: removed definitions - ensure not used?
  // TODO: removed enum values - ensure not used? - add required migrations defs
  // TODO: removed records fields - ensure not used? - add required migrations defs
  // TODO: definition update - has type not changed
  def apply(oldVersion: validated.Snapshot, newVersion: validated.Snapshot): F[Unit] = {
    oldVersion.hashCode()
    newVersion.hashCode()
    ().pure[F]
  }
}
