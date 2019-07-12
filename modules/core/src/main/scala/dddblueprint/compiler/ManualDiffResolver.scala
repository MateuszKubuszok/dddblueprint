package dddblueprint
package compiler

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.Sync
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached final class ManualDiffResolver[StateIO[_]: Sync: SnapshotState: SnapshotOperations] {

  def apply(migration: input.Migration): StateIO[Unit] =
    for {
      inputRefToDiffs <- manualDiffsByRefs(migration).pure[StateIO]
      ioRefs <- inputRefToDiffs.keys.map(ref => ref.translate[StateIO].map(ref -> _)).toList.sequence.map(_.toMap)
      _ <- SnapshotState[StateIO].modify {
        _.lens(_.manualDiffs).set(
          inputRefToDiffs.map {
            case (inputRef, diffs) => ioRefs(inputRef) -> diffs
          }
        )
      }
    } yield ()

  val translateActionToManualDiff: input.Action => (input.DefinitionRef, ListSet[output.ManualDiff]) = {
    case input.Action.CreateDefinition(definition)    => definition.ref -> ListSet.empty
    case input.Action.RemoveDefinition(definition)    => definition -> ListSet(output.ManualDiff.DefinitionRemoved)
    case input.Action.RenameDefinition(definition, _) => definition -> ListSet.empty
    case input.Action.AddEnumValues(definition, _)    => definition -> ListSet.empty
    case input.Action.RemoveEnumValues(definition, values) =>
      definition -> values.map(output.ManualDiff.EnumValueRemoved)
    case input.Action.RenameEnumValues(definition, _) => definition -> ListSet.empty
    case input.Action.AddRecordFields(definition, _)  => definition -> ListSet.empty
    case input.Action.RemoveRecordFields(definition, fields) =>
      definition -> fields.map(output.ManualDiff.RecordFieldRemoved)
    case input.Action.RenameRecordFields(definition, _) => definition -> ListSet.empty
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def manualDiffsByRefs(
    migration: input.Migration
  ): ListMap[input.DefinitionRef, NonEmptyList[output.ManualDiff]] = ListMap(
    migration.actions
      .map(translateActionToManualDiff)
      .zipWithIndex
      .groupBy(_._1._1)
      .mapValues { values =>
        values.map(_._2).min -> values.sortBy(_._2).flatMap(_._1._2)
      }
      .collect {
        case (ref, (i, head :: tail)) =>
          i -> (ref -> NonEmptyList(head, tail))
      }
      .toSeq
      .sortBy(_._1)
      .map(_._2): _*
  )
}

object ManualDiffResolver {

  @inline def apply[StateIO[_]](
    implicit ManualDiffResolver: ManualDiffResolver[StateIO]
  ): ManualDiffResolver[StateIO] = ManualDiffResolver
}
