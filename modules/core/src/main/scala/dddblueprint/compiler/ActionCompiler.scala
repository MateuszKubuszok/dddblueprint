package dddblueprint
package compiler

import cats.implicits._
import cats.{ Monad, Traverse }
import io.scalaland.pulp.Cached

// scalastyle:off no.whitespace.after.left.bracket
@Cached final class ActionCompiler[
  StateIO[_]: Monad: SchemaErrorRaise: SnapshotState: SnapshotOperations: ArgumentCompiler
] {
  // scalastyle:on no.whitespace.after.left.bracket

  private def mapFields(fields: input.Data.Definition.FieldSet): StateIO[output.Data.Definition.FieldSet] =
    Traverse[ListMap[String, *]].sequence[StateIO, output.Argument](fields.map {
      case (k, v) => k -> v.compile[StateIO]
    })

  def apply(action: input.Action): StateIO[Unit] = action match {
    case input.Action.CreateDefinition(definition)           => createDefinition(definition)
    case input.Action.RemoveDefinition(definition)           => removeDefinition(definition)
    case input.Action.RenameDefinition(definition, rename)   => renameDefinition(definition, rename)
    case input.Action.AddEnumValues(definition, values)      => addEnumValues(definition, values)
    case input.Action.RemoveEnumValues(definition, values)   => removeEnumValues(definition, values)
    case input.Action.RenameEnumValues(definition, rename)   => renameEnumValues(definition, rename)
    case input.Action.AddRecordFields(definition, fields)    => addRecordFields(definition, fields)
    case input.Action.RemoveRecordFields(definition, fields) => removeRecordFields(definition, fields)
    case input.Action.RenameRecordFields(definition, rename) => renameRecordFields(definition, rename)
  }

  lazy val createDefinition: input.Data.Definition => StateIO[Unit] = {
    case input.Data.Definition.Enum(ref, values, ttype) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalType = PrimitivesCompiler.enumerable(ttype)
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Enum(
            ref    = internalRef,
            values = values,
            `type` = internalType
          )
        )
      } yield ()

    case input.Data.Definition.Record.Entity(ref, fields) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalFields <- mapFields(fields)
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Record.Entity(
            ref    = internalRef,
            fields = internalFields
          )
        )
      } yield ()

    case input.Data.Definition.Record.Value(ref, fields) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalFields <- mapFields(fields)
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Record.Value(
            ref    = internalRef,
            fields = internalFields
          )
        )
      } yield ()

    case input.Data.Definition.Record.Event(ref, fields) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalFields <- mapFields(fields)
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Record.Event(
            ref    = internalRef,
            fields = internalFields
          )
        )
      } yield ()

    case input.Data.Definition.Service(ref, inputs, outputs) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalInput <- Traverse[ListSet]
          .sequence[StateIO, (String, output.Argument)](inputs.toListSet.map {
            case (k, v) => k.pure[StateIO].map2(v.compile[StateIO])(_ -> _)
          })
          .map(set => ListMap(set.toSeq: _*))
        internalOutput <- Traverse[ListSet].sequence[StateIO, output.DefinitionRef](
          outputs.map(_.requireExists[StateIO])
        )
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Service(
            ref    = internalRef,
            input  = internalInput,
            output = internalOutput
          )
        )
      } yield ()

    case input.Data.Definition.Publisher(ref, events) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalEvents <- Traverse[ListSet].sequence[StateIO, output.DefinitionRef](
          events.map(_.requireExists[StateIO])
        )
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Publisher(
            ref    = internalRef,
            events = internalEvents
          )
        )
      } yield ()

    case input.Data.Definition.Subscriber(ref, events) =>
      for {
        internalRef <- ref.requireNotExisted[StateIO]
        internalEvents <- Traverse[ListSet].sequence[StateIO, output.DefinitionRef](
          events.map(_.requireExists[StateIO])
        )
        _ <- internalRef.setDefinition[StateIO](
          output.Data.Definition.Subscriber(
            ref    = internalRef,
            events = internalEvents
          )
        )
      } yield ()
  }

  lazy val removeDefinition: input.DefinitionRef => StateIO[Unit] = ref =>
    for {
      internalRef <- ref.requireExists[StateIO]
      _ <- internalRef.removeDefinition[StateIO]
    } yield ()

  lazy val renameDefinition: (input.DefinitionRef, String) => StateIO[Unit] = (ref, rename) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      _ <- internalRef.renameDefinition[StateIO](rename)
    } yield ()

  lazy val addEnumValues: (input.DefinitionRef, ListSet[String]) => StateIO[Unit] = (ref, newValues) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          val common = oldValues.intersect(newValues)
          if (common.isEmpty) {
            SnapshotState[StateIO].modify(
              _.withDefinition(domainRef, internalRef, enum.withValues(oldValues ++ newValues))
            )
          } else {
            SchemaError.enumValuesExist(ref.domain.name, ref.name, common)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeEnumValues: (input.DefinitionRef, ListSet[String]) => StateIO[Unit] = (ref, removedValues) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          if (removedValues.subsetOf(oldValues)) {
            SnapshotState[StateIO].modify(
              _.withDefinition(domainRef, internalRef, enum.withValues(oldValues -- removedValues))
            )
          } else {
            SchemaError.enumValuesMissing(ref.domain.name, ref.name, removedValues -- oldValues)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val renameEnumValues: (input.DefinitionRef, ListMap[String, String]) => StateIO[Unit] = (ref, renamedValues) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          if (renamedValues.keySet.subsetOf(oldValues)) {
            SnapshotState[StateIO].modify(
              _.withDefinition(domainRef, internalRef, enum.withValues(oldValues.map { value =>
                renamedValues.getOrElse(value, value)
              }))
            )
          } else {
            SchemaError.enumValuesMissing(ref.domain.name, ref.name, renamedValues.keys.toListSet -- oldValues)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val addRecordFields: (input.DefinitionRef, input.Data.Definition.FieldSet) => StateIO[Unit] = (ref, newFields) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields, _)) =>
          mapFields(newFields).flatMap { internalNewFields =>
            val common = oldFields.keys.toListSet.intersect(newFields.keys.toListSet)
            if (common.isEmpty) {
              SnapshotState[StateIO].modify(
                _.withDefinition(domainRef, internalRef, record.withFields(oldFields ++ internalNewFields))
              )
            } else {
              SchemaError.recordFieldsExist(ref.domain.name, ref.name, common)
            }: StateIO[Unit]
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeRecordFields: (input.DefinitionRef, ListSet[String]) => StateIO[Unit] = (ref, removedFields) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields, _)) =>
          if (removedFields.subsetOf(oldFields.keys.toListSet)) {
            SnapshotState[StateIO].modify(
              _.withDefinition(domainRef, internalRef, record.withFields(oldFields.filter {
                case (k, _) => !removedFields.contains(k)
              }))
            )
          } else {
            SchemaError.recordFieldsMissing(ref.domain.name, ref.name, removedFields -- oldFields.keys)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val renameRecordFields: (input.DefinitionRef, ListMap[String, String]) => StateIO[Unit] = (ref, renamedFields) =>
    for {
      internalRef <- ref.requireExists[StateIO]
      domainRef <- internalRef.toDomain[StateIO]
      currentDefinitionOpt <- internalRef.getDefinition[StateIO]
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields, _)) =>
          if (renamedFields.keySet.subsetOf(oldFields.keys.toListSet)) {
            SnapshotState[StateIO].modify(
              _.withDefinition(domainRef, internalRef, record.withFields(oldFields.map {
                case (k, v) => renamedFields.getOrElse(k, k) -> v
              }))
            )
          } else {
            SchemaError.recordFieldsMissing(ref.domain.name, ref.name, renamedFields.keys.toListSet -- oldFields.keys)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()
}

object ActionCompiler {

  @inline def apply[StateIO[_]](implicit actionCompiler: ActionCompiler[StateIO]): ActionCompiler[StateIO] =
    actionCompiler
}
