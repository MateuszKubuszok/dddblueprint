package dddblueprint
package compiler

import cats.implicits._
import cats.Monad

import scala.collection.immutable.ListSet

class ActionCompiler[F[_]: Monad: SchemaErrorRaise: SnapshotState](snapshotOperations: SnapshotOperations[F]) {

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
        internalRef <- snapshotOperations.translateDefinitionRef(ref)
        definitionExists <- snapshotOperations.hasDefinition(internalRef)
        _ <- if (definitionExists) SchemaError.definitionExists[F, Unit](domain = ref.domain.name, name = ref.name)
        else ().pure[F]
        internalType = PrimitivesCompiler.enumerable(ttype)
        _ <- snapshotOperations.setDefinition(internalRef,
                                              validated.Data.Definition.Enum(
                                                ref    = internalRef,
                                                values = values,
                                                `type` = internalType
                                              ))
      } yield ()

    // TODO: other types
    case _ =>
      ???
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
