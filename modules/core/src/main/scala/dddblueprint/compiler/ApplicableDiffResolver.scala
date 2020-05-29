package dddblueprint
package compiler

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.Sync
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached final class ApplicableDiffResolver[StateIO[_]: Sync: SnapshotState: SnapshotOperations] {

  def apply(migration: input.Migration): StateIO[Unit] =
    for {
      inputRefToDiffs <- applicableDiffsByRefs(migration).pure[StateIO]
      ioRefs <- inputRefToDiffs.keys.map(ref => ref.translate[StateIO].map(ref -> _)).toList.sequence.map(_.toMap)
      _ <- SnapshotState[StateIO].modify {
        _.lens(_.applicableDiffs).set(
          inputRefToDiffs.map {
            case (inputRef, diffs) => ioRefs(inputRef) -> diffs
          }
        )
      }
    } yield ()

  val translateActionToApplicableDiff: input.Action => (input.DefinitionRef, ListSet[output.ApplicableDiff]) = {
    case input.Action.CreateDefinition(definition) => definition.ref -> ListSet.empty
    case input.Action.RemoveDefinition(definition) => definition -> ListSet.empty
    case input.Action.RenameDefinition(definition, rename) =>
      definition -> ListSet(output.ApplicableDiff.DefinitionRenamed(rename))
    case input.Action.AddEnumValues(definition, _)    => definition -> ListSet.empty
    case input.Action.RemoveEnumValues(definition, _) => definition -> ListSet.empty
    case input.Action.RenameEnumValues(definition, rename) =>
      definition -> rename.map(output.ApplicableDiff.EnumValueRenamed.tupled(_): output.ApplicableDiff).toListSet
    case input.Action.AddRecordFields(definition, _) => definition -> ListSet.empty
    case input.Action.RemoveRecordFields(definition, fields) =>
      definition -> fields.map(output.ApplicableDiff.FieldRemoved)
    case input.Action.RenameRecordFields(definition, rename) =>
      definition -> rename.map(output.ApplicableDiff.FieldRenamed.tupled(_): output.ApplicableDiff).toListSet
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def applicableDiffsByRefs(
    migration: input.Migration
  ): ListMap[input.DefinitionRef, NonEmptyList[output.ApplicableDiff]] = ListMap(
    migration.actions
      .map(translateActionToApplicableDiff)
      .zipWithIndex
      .groupBy(_._1._1)
      .mapValues(values => values.map(_._2).min -> values.sortBy(_._2).flatMap(_._1._2))
      .collect {
        case (ref, (i, head :: tail)) =>
          i -> (ref -> NonEmptyList(head, tail))
      }
      .toSeq
      .sortBy(_._1)
      .map(_._2): _*
  )
}

object ApplicableDiffResolver {

  @inline def apply[StateIO[_]](
    implicit applicableDiffResolver: ApplicableDiffResolver[StateIO]
  ): ApplicableDiffResolver[StateIO] = applicableDiffResolver
}
