package dddblueprint
package compiler

import cats.implicits._
import cats.{ Monad, Traverse }

import scala.collection.immutable.{ ListMap, ListSet }

class ActionCompiler[F[_]: Monad: SchemaErrorRaise: SnapshotState](snapshotOperations: SnapshotOperations[F]) {

  private def definitionExists(ref: schema.DefinitionRef): F[validated.DefinitionRef] =
    for {
      internalRef <- snapshotOperations.translateDefinitionRef(ref)
      definitionExists <- snapshotOperations.hasDefinition(internalRef)
      _ <- if (!definitionExists) SchemaError.definitionMissing[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

  private def definitionNotExists(ref: schema.DefinitionRef): F[validated.DefinitionRef] =
    for {
      internalRef <- snapshotOperations.translateDefinitionRef(ref)
      definitionExists <- snapshotOperations.hasDefinition(internalRef)
      _ <- if (definitionExists) SchemaError.definitionExists[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

  // TODO: implement missing data
  private val mapData: schema.Data => F[validated.Data] = {
    case p: schema.Data.Primitive => (PrimitivesCompiler(p): validated.Data).pure[F]
    case _ => ??? // TODO: implement - assume it was already defined when used
  }

  def apply(action: schema.Action): F[Unit] = action match {
    case schema.Action.CreateDefinition(definition)           => createDefinition(definition)
    case schema.Action.RemoveDefinition(definition)           => removeDefinition(definition)
    case schema.Action.AddEnumValues(definition, values)      => addEnumValues(definition, values)
    case schema.Action.RemoveEnumValues(definition, values)   => removeEnumValues(definition, values)
    case schema.Action.AddRecordFields(definition, fields)    => addRecordFields(definition, fields)
    case schema.Action.RemoveRecordFields(definition, fields) => removeRecordFields(definition, fields)
  }

  val createDefinition: schema.Data.Definition => F[Unit] = {
    case schema.Data.Definition.Enum(ref, values, ttype) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalType = PrimitivesCompiler.enumerable(ttype)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Enum(
                                                ref    = internalRef,
                                                values = values,
                                                `type` = internalType
                                              ))
      } yield ()

    case schema.Data.Definition.Record.Tuple(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- Traverse[ListMap[String, ?]].sequence[F, validated.Data](fields.map {
          case (k, v) => k -> mapData(v)
        })
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Record.Tuple(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case schema.Data.Definition.Record.Entity(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- Traverse[ListMap[String, ?]].sequence[F, validated.Data](fields.map {
          case (k, v) => k -> mapData(v)
        })
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Record.Entity(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case schema.Data.Definition.Record.Value(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- Traverse[ListMap[String, ?]].sequence[F, validated.Data](fields.map {
          case (k, v) => k -> mapData(v)
        })
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Record.Value(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case schema.Data.Definition.Record.Event(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- Traverse[ListMap[String, ?]].sequence[F, validated.Data](fields.map {
          case (k, v) => k -> mapData(v)
        })
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Record.Event(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case schema.Data.Definition.Service(ref, input, output) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalInput <- Traverse[ListSet].sequence[F, validated.DefinitionRef](input.map(definitionExists))
        internalOutput <- Traverse[ListSet].sequence[F, validated.DefinitionRef](output.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Service(
                                                ref    = internalRef,
                                                input  = internalInput,
                                                output = internalOutput
                                              ))
      } yield ()

    case schema.Data.Definition.Publisher(ref, events) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, validated.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Publisher(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()

    case schema.Data.Definition.Subscriber(ref, events) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, validated.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Subscriber(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()
  }

  val removeDefinition: schema.DefinitionRef => F[Unit] = ref =>
    for {
      internalRef <- definitionExists(ref)
      _ <- snapshotOperations.removeDefinition(internalRef)
    } yield ()

  val addEnumValues: (schema.DefinitionRef, ListSet[String]) => F[Unit] = (ref, newValues) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ validated.Data.Definition.Enum(_, oldValues, _)) =>
          val common = oldValues.intersect(newValues)
          if (common.isEmpty) enum.withValues(oldValues ++ newValues).pure[F]
          else SchemaError.enumValuesExist[F, validated.Data.Definition.Enum](ref.domain.name, ref.name, common)
        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, validated.Data.Definition.Enum](ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  val removeEnumValues: (schema.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedValues) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ validated.Data.Definition.Enum(_, oldValues, _)) =>
          if (removedValues.subsetOf(oldValues)) {
            enum.withValues(oldValues -- removedValues).pure[F]
          } else {
            SchemaError.enumValuesMissing[F, validated.Data.Definition.Enum](ref.domain.name,
                                                                             ref.name,
                                                                             removedValues -- oldValues)
          }
        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, validated.Data.Definition.Enum](ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  val addRecordFields: (schema.DefinitionRef, schema.Data.Definition.FieldSet) => F[Unit] = (ref, newFields) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ validated.Data.Definition.Record.Aux(_, oldFields)) =>
          Traverse[ListMap[String, ?]]
            .sequence[F, validated.Data](newFields.map { case (k, v) => k -> mapData(v) })
            .flatMap { internalNewFields =>
              val common = oldFields.keys.to[ListSet].intersect(newFields.keys.to[ListSet])
              if (common.isEmpty) record.withFields(oldFields ++ internalNewFields).pure[F]
              else SchemaError.recordFieldsExist[F, validated.Data.Definition.Record](ref.domain.name, ref.name, common)
            }

        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, validated.Data.Definition.Record](ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  val removeRecordFields: (schema.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedFields) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ validated.Data.Definition.Record.Aux(_, oldFields)) =>
          if (removedFields.subsetOf(oldFields.keys.to[ListSet])) {
            record.withFields(oldFields.filter { case (k, _) => !removedFields.contains(k) }).pure[F]
          } else {
            SchemaError.recordFieldsMissing[F, validated.Data.Definition.Record](ref.domain.name,
                                                                                 ref.name,
                                                                                 removedFields -- oldFields.keys)
          }

        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, validated.Data.Definition.Record](ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()
}
