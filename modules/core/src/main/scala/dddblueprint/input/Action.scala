package dddblueprint
package input

import cats.Eq
import cats.derived.ShowPretty
import cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) sealed trait Action extends ADT
object Action {

  @Semi(Eq, ShowPretty) final case class CreateDefinition(definition: Data.Definition) extends Action
  @Semi(Eq, ShowPretty) final case class RemoveDefinition(definition: DefinitionRef) extends Action
  @Semi(Eq, ShowPretty) final case class RenameDefinition(definition: DefinitionRef, rename: String) extends Action
  @Semi(Eq, ShowPretty) final case class AddEnumValues(definition:    DefinitionRef, values: ListSet[String])
      extends Action
  @Semi(Eq, ShowPretty) final case class RemoveEnumValues(definition: DefinitionRef, values: ListSet[String])
      extends Action
  @Semi(Eq, ShowPretty) final case class RenameEnumValues(definition: DefinitionRef, rename: ListMap[String, String])
      extends Action
  @Semi(Eq, ShowPretty) final case class AddRecordFields(definition: DefinitionRef, fields: Data.Definition.FieldSet)
      extends Action
  @Semi(Eq, ShowPretty) final case class RemoveRecordFields(definition: DefinitionRef, fields: ListSet[String])
      extends Action
  @Semi(Eq, ShowPretty) final case class RenameRecordFields(definition: DefinitionRef, rename: ListMap[String, String])
      extends Action
}
