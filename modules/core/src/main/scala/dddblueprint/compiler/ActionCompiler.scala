package dddblueprint
package compiler

import cats.implicits._
import cats.{ Monad, Traverse }
import io.scalaland.pulp.Cached

import scala.collection.immutable.{ ListMap, ListSet }

@Cached class ActionCompiler[F[_]: Monad: SchemaErrorRaise: SnapshotState: SnapshotOperations: ArgumentCompiler] {

  private def mapFields(fields: input.Data.Definition.FieldSet): F[output.Data.Definition.FieldSet] =
    Traverse[ListMap[String, ?]].sequence[F, output.Argument](fields.map {
      case (k, v) => k -> ArgumentCompiler[F].apply(v, createDefinition)
    })

  def apply(action: input.Action): F[Unit] = action match {
    case input.Action.CreateDefinition(definition)           => createDefinition(definition)
    case input.Action.RemoveDefinition(definition)           => removeDefinition(definition)
    case input.Action.AddEnumValues(definition, values)      => addEnumValues(definition, values)
    case input.Action.RemoveEnumValues(definition, values)   => removeEnumValues(definition, values)
    case input.Action.AddRecordFields(definition, fields)    => addRecordFields(definition, fields)
    case input.Action.RemoveRecordFields(definition, fields) => removeRecordFields(definition, fields)
  }

  lazy val createDefinition: input.Data.Definition => F[Unit] = {
    case input.Data.Definition.Enum(ref, values, ttype) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalType = PrimitivesCompiler.enumerable(ttype)
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Enum(
                                                   ref    = internalRef,
                                                   values = values,
                                                   `type` = internalType
                                                 ))
      } yield ()

    case input.Data.Definition.Record.Tuple(ref, fields) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Record.Tuple(
                                                   ref    = internalRef,
                                                   fields = internalFields
                                                 ))
      } yield ()

    case input.Data.Definition.Record.Entity(ref, fields) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Record.Entity(
                                                   ref    = internalRef,
                                                   fields = internalFields
                                                 ))
      } yield ()

    case input.Data.Definition.Record.Value(ref, fields) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Record.Value(
                                                   ref    = internalRef,
                                                   fields = internalFields
                                                 ))
      } yield ()

    case input.Data.Definition.Record.Event(ref, fields) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Record.Event(
                                                   ref    = internalRef,
                                                   fields = internalFields
                                                 ))
      } yield ()

    case input.Data.Definition.Service(ref, inputs, outputs) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalInput <- Traverse[ListSet]
          .sequence[F, (String, output.Argument)](inputs.to[ListSet].map {
            case (k, v) => k.pure[F].map2(ArgumentCompiler[F].apply(v, createDefinition))(_ -> _)
          })
          .map(set => ListMap(set.toSeq: _*))
        internalOutput <- Traverse[ListSet].sequence[F, output.DefinitionRef](
          outputs.map(SnapshotOperations[F].definitionExists)
        )
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Service(
                                                   ref    = internalRef,
                                                   input  = internalInput,
                                                   output = internalOutput
                                                 ))
      } yield ()

    case input.Data.Definition.Publisher(ref, events) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, output.DefinitionRef](
          events.map(SnapshotOperations[F].definitionExists)
        )
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Publisher(
                                                   ref    = internalRef,
                                                   events = internalEvents
                                                 ))
      } yield ()

    case input.Data.Definition.Subscriber(ref, events) =>
      for {
        internalRef <- SnapshotOperations[F].definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, output.DefinitionRef](
          events.map(SnapshotOperations[F].definitionExists)
        )
        _ <- SnapshotOperations[F].setDefinition(internalRef,
                                                 output.Data.Definition.Subscriber(
                                                   ref    = internalRef,
                                                   events = internalEvents
                                                 ))
      } yield ()
  }

  lazy val removeDefinition: input.DefinitionRef => F[Unit] = ref =>
    for {
      internalRef <- SnapshotOperations[F].definitionExists(ref)
      _ <- SnapshotOperations[F].removeDefinition(internalRef)
    } yield ()

  lazy val addEnumValues: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, newValues) =>
    for {
      internalRef <- SnapshotOperations[F].definitionExists(ref)
      domainRef <- SnapshotOperations[F].definitionToDomain(internalRef)
      currentDefinitionOpt <- SnapshotOperations[F].getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          val common = oldValues.intersect(newValues)
          if (common.isEmpty) {
            SnapshotState[F].modify(_.withDefinition(domainRef, internalRef, enum.withValues(oldValues ++ newValues)))
          } else {
            SchemaError.enumValuesExist(ref.domain.name, ref.name, common)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeEnumValues: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedValues) =>
    for {
      internalRef <- SnapshotOperations[F].definitionExists(ref)
      domainRef <- SnapshotOperations[F].definitionToDomain(internalRef)
      currentDefinitionOpt <- SnapshotOperations[F].getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          if (removedValues.subsetOf(oldValues)) {
            SnapshotState[F].modify(
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

  lazy val addRecordFields: (input.DefinitionRef, input.Data.Definition.FieldSet) => F[Unit] = (ref, newFields) =>
    for {
      internalRef <- SnapshotOperations[F].definitionExists(ref)
      domainRef <- SnapshotOperations[F].definitionToDomain(internalRef)
      currentDefinitionOpt <- SnapshotOperations[F].getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields, _)) =>
          mapFields(newFields).flatMap { internalNewFields =>
            val common = oldFields.keys.to[ListSet].intersect(newFields.keys.to[ListSet])
            if (common.isEmpty) {
              SnapshotState[F].modify(
                _.withDefinition(domainRef, internalRef, record.withFields(oldFields ++ internalNewFields))
              )
            } else {
              SchemaError.recordFieldsExist(ref.domain.name, ref.name, common)
            }: F[Unit]
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch(ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeRecordFields: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedFields) =>
    for {
      internalRef <- SnapshotOperations[F].definitionExists(ref)
      domainRef <- SnapshotOperations[F].definitionToDomain(internalRef)
      currentDefinitionOpt <- SnapshotOperations[F].getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields, _)) =>
          if (removedFields.subsetOf(oldFields.keys.to[ListSet])) {
            SnapshotState[F].modify(
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
}
