package dddblueprint
package compiler

import cats.data.Validated.Valid
import monocle.macros.syntax.lens._

object HistoryCompiler extends (schema.History => Result[validated.Blueprint]) {

  val emptyBlueprintSnapshot: Result[(validated.Blueprint, validated.Snapshot)] = Result.valid(validated.Blueprint() -> validated.Snapshot())

  def apply(history: schema.History): Result[validated.Blueprint] =
    history.migrations
      .foldLeft(emptyBlueprintSnapshot) {
        case (Valid((previousBlueprint, previousSnapshot)), migration) =>
          MigrationCompiler(previousSnapshot, migration).map { newSnapshot =>
            previousBlueprint.lens(_.versions).modify(_ :+ newSnapshot) -> newSnapshot
          }
        case (invalid, _) => invalid
      }
      .map(_._1)
}
