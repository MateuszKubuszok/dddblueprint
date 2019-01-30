package dddblueprint
package compiler

import cats.implicits._

object MigrationCompiler extends ((validated.Snapshot, schema.Migration) => Result[validated.Snapshot]) {

  def apply(oldVersion: validated.Snapshot, migration: schema.Migration): Result[validated.Snapshot] =
    migration.actions
      .foldLeft(oldVersion -> Result.valid(oldVersion)) {
        case ((lastValid, currentState), action) =>
          // aggregates old errors with new ones that could appear for action(lastValid)
          val nextState = (currentState, ActionCompiler(currentState.getOrElse(lastValid), action)).mapN {
            case (_, newer) => newer
          }
          nextState.getOrElse(lastValid) -> nextState
      }
      ._2
}
