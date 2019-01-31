package dddblueprint
package schema

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.ListSet

@Semi(Eq, Show) sealed trait Action extends ADT
object Action {

  @Semi(Eq, Show) final case class CreateDefinition(definition: Data.Definition) extends Action
  @Semi(Eq, Show) final case class RemoveDefinition(definition: DefinitionRef) extends Action
  @Semi(Eq, Show) final case class AddEnumValues(definition:    DefinitionRef, values: ListSet[String]) extends Action
  @Semi(Eq, Show) final case class RemoveEnumValues(definition: DefinitionRef, values: ListSet[String]) extends Action
  @Semi(Eq, Show) final case class AddRecordFields(definition:  DefinitionRef, fields: Data.Definition.FieldSet)
      extends Action
  @Semi(Eq, Show) final case class RemoveRecordFields(definition: DefinitionRef, fields: ListSet[String]) extends Action
}
