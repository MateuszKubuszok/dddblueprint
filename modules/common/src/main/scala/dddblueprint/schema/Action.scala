package dddblueprint
package schema

import scala.collection.immutable.ListSet

sealed trait Action extends ADT
object Action {

  final case class CreateDefinition(definition:   Data.Domain) extends Action
  final case class RemoveDefinition(definition:   DefinitionRef) extends Action
  final case class AddEnumValues(definition:      DefinitionRef, values: ListSet[String]) extends Action
  final case class RemoveEnumValues(definition:   DefinitionRef, values: ListSet[String]) extends Action
  final case class AddRecordFields(definition:    DefinitionRef, fields: Data.Domain.FieldSet) extends Action
  final case class RemoveRecordFields(definition: DefinitionRef, fields: ListSet[String]) extends Action
}
