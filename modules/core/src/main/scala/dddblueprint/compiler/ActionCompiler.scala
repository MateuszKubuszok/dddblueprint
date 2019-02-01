package dddblueprint
package compiler

import cats.implicits._
import cats.{ Monad, Traverse }
import io.scalaland.pulp.Cached

import scala.collection.immutable.{ ListMap, ListSet }

@Cached class ActionCompiler[F[_]: Monad: SchemaErrorRaise: SnapshotState](snapshotOperations: SnapshotOperations[F]) {

  private def definitionExists(ref: input.DefinitionRef): F[output.DefinitionRef] =
    for {
      internalRef <- snapshotOperations.translateDefinitionRef(ref)
      definitionExists <- snapshotOperations.hasDefinition(internalRef)
      _ <- if (!definitionExists) SchemaError.definitionMissing[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

  private def definitionNotExists(ref: input.DefinitionRef): F[output.DefinitionRef] =
    for {
      internalRef <- snapshotOperations.translateDefinitionRef(ref)
      definitionExists <- snapshotOperations.hasDefinition(internalRef)
      _ <- if (definitionExists) SchemaError.definitionExists[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

  private lazy val mapArgument: input.Argument => F[output.Argument] = {
    case ref: input.DefinitionRef => snapshotOperations.translateDefinitionRef(ref).map(r => r: output.Argument)

    case p: input.Data.Primitive => (PrimitivesCompiler(p): output.Argument).pure[F]

    case tuple @ input.Data.Definition.Record.Tuple(ref, _) =>
      for {
        internalRef <- definitionNotExists(ref)
        _ <- createDefinition(tuple)
      } yield internalRef: output.Argument
  }

  private def mapFields(fields: input.Data.Definition.FieldSet): F[output.Data.Definition.Record.FieldSet] =
    Traverse[ListMap[String, ?]].sequence[F, output.Argument](fields.map {
      case (k, v) => k -> mapArgument(v)
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
        internalRef <- definitionNotExists(ref)
        internalType = PrimitivesCompiler.enumerable(ttype)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Enum(
                                                ref    = internalRef,
                                                values = values,
                                                `type` = internalType
                                              ))
      } yield ()

    case input.Data.Definition.Record.Tuple(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Record.Tuple(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case input.Data.Definition.Record.Entity(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Record.Entity(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case input.Data.Definition.Record.Value(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Record.Value(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case input.Data.Definition.Record.Event(ref, fields) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalFields <- mapFields(fields)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Record.Event(
                                                ref    = internalRef,
                                                fields = internalFields
                                              ))
      } yield ()

    case input.Data.Definition.Service(ref, inputs, outputs) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalInput <- Traverse[ListSet].sequence[F, output.DefinitionRef](inputs.map(definitionExists))
        internalOutput <- Traverse[ListSet].sequence[F, output.DefinitionRef](outputs.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Service(
                                                ref    = internalRef,
                                                input  = internalInput,
                                                output = internalOutput
                                              ))
      } yield ()

    case input.Data.Definition.Publisher(ref, events) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, output.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Publisher(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()

    case input.Data.Definition.Subscriber(ref, events) =>
      for {
        internalRef <- definitionNotExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, output.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              output.Data.Definition.Subscriber(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()
  }

  lazy val removeDefinition: input.DefinitionRef => F[Unit] = ref =>
    for {
      internalRef <- definitionExists(ref)
      _ <- snapshotOperations.removeDefinition(internalRef)
    } yield ()

  lazy val addEnumValues: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, newValues) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          val common = oldValues.intersect(newValues)
          if (common.isEmpty) enum.withValues(oldValues ++ newValues).pure[F]
          else SchemaError.enumValuesExist[F, output.Data.Definition.Enum](ref.domain.name, ref.name, common)
        case Some(other) =>
          SchemaError.definitionTypeMismatch[F, output.Data.Definition.Enum](ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeEnumValues: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedValues) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(enum @ output.Data.Definition.Enum(_, oldValues, _)) =>
          if (removedValues.subsetOf(oldValues)) {
            enum.withValues(oldValues -- removedValues).pure[F]
          } else {
            SchemaError
              .enumValuesMissing[F, output.Data.Definition.Enum](ref.domain.name, ref.name, removedValues -- oldValues)
          }
        case Some(other) =>
          SchemaError.definitionTypeMismatch[F, output.Data.Definition.Enum](ref.domain.name, ref.name, "enum", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val addRecordFields: (input.DefinitionRef, input.Data.Definition.FieldSet) => F[Unit] = (ref, newFields) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields)) =>
          mapFields(newFields).flatMap { internalNewFields =>
            val common = oldFields.keys.to[ListSet].intersect(newFields.keys.to[ListSet])
            if (common.isEmpty) record.withFields(oldFields ++ internalNewFields).pure[F]
            else SchemaError.recordFieldsExist[F, output.Data.Definition.Record](ref.domain.name, ref.name, common)
          }

        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, output.Data.Definition.Record](ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()

  lazy val removeRecordFields: (input.DefinitionRef, ListSet[String]) => F[Unit] = (ref, removedFields) =>
    for {
      internalRef <- definitionExists(ref)
      currentDefinitionOpt <- snapshotOperations.getDefinition(internalRef)
      _ <- currentDefinitionOpt match {
        case Some(record @ output.Data.Definition.Record.Aux(_, oldFields)) =>
          if (removedFields.subsetOf(oldFields.keys.to[ListSet])) {
            record.withFields(oldFields.filter { case (k, _) => !removedFields.contains(k) }).pure[F]
          } else {
            SchemaError.recordFieldsMissing[F, output.Data.Definition.Record](ref.domain.name,
                                                                              ref.name,
                                                                              removedFields -- oldFields.keys)
          }
        case Some(other) =>
          SchemaError
            .definitionTypeMismatch[F, output.Data.Definition.Record](ref.domain.name, ref.name, "record", other)
        case None =>
          SchemaError.definitionMissing(ref.domain.name, ref.name)
      }
    } yield ()
}
