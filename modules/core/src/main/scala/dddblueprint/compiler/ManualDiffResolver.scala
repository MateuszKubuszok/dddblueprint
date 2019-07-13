package dddblueprint
package compiler

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.Sync
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached final class ManualDiffResolver[StateIO[_]: Sync: SnapshotState: SnapshotOperations: ArgumentCompiler] {

  def apply(migration: input.Migration): StateIO[Unit] =
    for {
      inputRefToDiffs <- manualDiffsByRefs(migration)
      ioRefs <- inputRefToDiffs.keys.map(ref => ref.translate[StateIO].map(ref -> _)).toList.sequence.map(_.toMap)
      _ <- SnapshotState[StateIO].modify {
        _.lens(_.manualDiffs).set(
          inputRefToDiffs.map {
            case (inputRef, diffs) => ioRefs(inputRef) -> diffs
          }
        )
      }
    } yield ()

  // scalastyle:off cyclomatic.complexity
  def translateActionToManualDiff: input.Action => StateIO[(input.DefinitionRef, ListSet[output.ManualDiff])] = {
    case input.Action.CreateDefinition(definition) => (definition.ref -> ListSet.empty[output.ManualDiff]).pure[StateIO]
    case input.Action.RemoveDefinition(definition) =>
      (definition -> ListSet[output.ManualDiff](output.ManualDiff.DefinitionRemoved)).pure[StateIO]
    case input.Action.RenameDefinition(definition, _) => (definition -> ListSet.empty[output.ManualDiff]).pure[StateIO]
    case input.Action.AddEnumValues(definition, _)    => (definition -> ListSet.empty[output.ManualDiff]).pure[StateIO]
    case input.Action.RemoveEnumValues(definition, values) =>
      (definition -> values.map(output.ManualDiff.EnumValueRemoved(_): output.ManualDiff)).pure[StateIO]
    case input.Action.RenameEnumValues(definition, _) => (definition -> ListSet.empty[output.ManualDiff]).pure[StateIO]
    case input.Action.AddRecordFields(definition, fields) =>
      fields
        .to[List]
        .traverse {
          case (name, arg) =>
            arg.compile[StateIO].map(output.ManualDiff.RecordFieldAdded(name, _): output.ManualDiff)
        }
        .map(list => definition -> list.to[ListSet])
    case input.Action.RemoveRecordFields(definition, _) =>
      (definition -> ListSet.empty[output.ManualDiff]).pure[StateIO]
    case input.Action.RenameRecordFields(definition, _) =>
      (definition -> ListSet.empty[output.ManualDiff]).pure[StateIO]
  }
  // scalastyle:on cyclomatic.complexity

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def manualDiffsByRefs(
    migration: input.Migration
  ): StateIO[ListMap[input.DefinitionRef, NonEmptyList[output.ManualDiff]]] =
    migration.actions.traverse(translateActionToManualDiff).map { diffs =>
      ListMap(
        diffs.zipWithIndex
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
}

object ManualDiffResolver {

  @inline def apply[StateIO[_]](
    implicit ManualDiffResolver: ManualDiffResolver[StateIO]
  ): ManualDiffResolver[StateIO] = ManualDiffResolver
}
