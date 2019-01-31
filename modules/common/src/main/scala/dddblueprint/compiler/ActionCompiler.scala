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
      _ <- if (definitionExists) SchemaError.definitionExists[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
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
        internalRef <- definitionExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, validated.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Publisher(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()

    case schema.Data.Definition.Subscriber(ref, events) =>
      for {
        internalRef <- definitionExists(ref)
        internalEvents <- Traverse[ListSet].sequence[F, validated.DefinitionRef](events.map(definitionExists))
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Subscriber(
                                                ref    = internalRef,
                                                events = internalEvents
                                              ))
      } yield ()
  }

  // TODO: check if definition exists (should)
  def removeDefinition(definition: schema.DefinitionRef): F[Unit] =
    ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should not)
  def addEnumValues(definition: schema.DefinitionRef, values: ListSet[String]): F[Unit] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should)
  def removeEnumValues(definition: schema.DefinitionRef, values: ListSet[String]): F[Unit] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should not)
  def addRecordFields(definition: schema.DefinitionRef, fields: schema.Data.Definition.FieldSet): F[Unit] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should)
  def removeRecordFields(definition: schema.DefinitionRef, fields: ListSet[String]): F[Unit] = ???
}
