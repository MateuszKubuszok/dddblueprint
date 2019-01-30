package dddblueprint
package compiler

import scala.collection.immutable.ListSet

object ActionCompiler extends ((validated.Snapshot, schema.Action) => Result[validated.Snapshot]) {

  def apply(oldVersion: validated.Snapshot, action: schema.Action): Result[validated.Snapshot] = action match {
    case schema.Action.CreateDefinition(definition)           => createDefinition(oldVersion, definition)
    case schema.Action.RemoveDefinition(definition)           => removeDefinition(oldVersion, definition)
    case schema.Action.AddEnumValues(definition, values)      => addEnumValues(oldVersion, definition, values)
    case schema.Action.RemoveEnumValues(definition, values)   => removeEnumValues(oldVersion, definition, values)
    case schema.Action.AddRecordFields(definition, fields)    => addRecordFields(oldVersion, definition, fields)
    case schema.Action.RemoveRecordFields(definition, fields) => removeRecordFields(oldVersion, definition, fields)
  }

  // TODO: check if definition exists (should not)
  def createDefinition(oldVersion: validated.Snapshot, definition: schema.Data.Domain): Result[validated.Snapshot] = ???

  // TODO: check if definition exists (should)
  def removeDefinition(oldVersion: validated.Snapshot, definition: schema.DefinitionRef): Result[validated.Snapshot] =
    ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should not)
  def addEnumValues(oldVersion: validated.Snapshot,
                    definition: schema.DefinitionRef,
                    values:     ListSet[String]): Result[validated.Snapshot] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should)
  def removeEnumValues(oldVersion: validated.Snapshot,
                       definition: schema.DefinitionRef,
                       values:     ListSet[String]): Result[validated.Snapshot] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should not)
  def addRecordFields(oldVersion: validated.Snapshot,
                      definition: schema.DefinitionRef,
                      fields:     schema.Data.Domain.FieldSet): Result[validated.Snapshot] = ???

  // TODO: check if definition exist (should)
  // TODO: check if value exist (should)
  def removeRecordFields(oldVersion: validated.Snapshot,
                         definition: schema.DefinitionRef,
                         fields:     ListSet[String]): Result[validated.Snapshot] = ???
}
