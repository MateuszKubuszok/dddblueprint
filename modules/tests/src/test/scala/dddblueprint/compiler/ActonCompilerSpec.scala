package dddblueprint
package compiler

import cats.data.NonEmptyList
import io.scalaland.pulp.Provider
import monocle.macros.syntax.lens._
import org.specs2.specification.Scope

import scala.collection.immutable.{ ListMap, ListSet }

class ActonCompilerSpec extends CompilerSpec {

  "ActionCompiler on CreateDefinition" should {

    "correctly compile enum definition" in new Fixture {
      val definition       = inputs.Data.Definition.Enum1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Enum1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile tuple definition" in new Fixture {
      val definition       = inputs.Data.Definition.Record.Tuple1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Tuple1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile entity definition" in new Fixture {
      val definition       = inputs.Data.Definition.Record.Entity1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Entity1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile value definition" in new Fixture {
      val definition       = inputs.Data.Definition.Record.Value1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Value1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile event definition" in new Fixture {
      val definition       = inputs.Data.Definition.Record.Event1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Event1.lens(_.ref).set(internalRef)
      }
    }

    "raise error if definition already exists" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(outputs.Domain1Ref,
                        inputs.Domain1Ref.name,
                        outputs.Enum1Ref,
                        inputs.Enum1Ref.name,
                        outputs.Data.Definition.Enum1)
      val definition       = inputs.Data.Definition.Enum1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionExists(inputs.Domain1Ref.name, inputs.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RemoveDefinition" should {

    "correctly remove definition" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val removeDefinition = input.Action.RemoveDefinition(inputs.Enum1Ref)

      new TestSnapshot(ActionCompiler(removeDefinition))(snapshot) {
        val internalRef = definitionRefIso.get(removeDefinition.definition)
        snapshot.definitions.get(internalRef) === None
      }
    }

    "raise error if definition doesn't exist" in new Fixture {
      val snapshot         = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val removeDefinition = input.Action.RemoveDefinition(inputs.Enum1Ref)

      new TestSnapshot(ActionCompiler(removeDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RenameDefinition" should {

    "correctly rename definition" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val renameDefinition = input.Action.RenameDefinition(inputs.Enum1Ref, "renamed")

      new TestSnapshot(ActionCompiler(renameDefinition))(snapshot) {
        val internalRef = definitionRefIso.get(inputs.Enum1Ref.copy(name = "renamed"))
        snapshot.definitions(internalRef) === outputs.Data.Definition.Enum1.lens(_.ref).set(internalRef)
      }
    }

    "raise error if new name is already taken" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum2Ref,
          "rename",
          outputs.Data.Definition.Enum2
        )
      val renameDefinition = input.Action.RenameDefinition(inputs.Enum1Ref, "rename")

      new TestSnapshot(ActionCompiler(renameDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionExists(inputs.Domain1Ref.name, "rename"))
          )
        )
      }
    }

    "raise error if definition doesn't exist" in new Fixture {
      val snapshot         = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val removeDefinition = input.Action.RenameDefinition(inputs.Enum1Ref, "renamed")

      new TestSnapshot(ActionCompiler(removeDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on AddEnumValues" should {

    "add values if enum exists and none of the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val addEnumValues = input.Action.AddEnumValues(inputs.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        val internalRef = definitionRefIso.get(addEnumValues.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Enum1.withValues(ListSet("a", "b", "c", "d"))
      }
    }

    "raise error if enum exists and some the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1.withValues(ListSet("a", "b", "c"))
        )
      val addEnumValues = input.Action.AddEnumValues(inputs.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.EnumValuesExist(
                inputs.Domain1Ref.name,
                inputs.Enum1Ref.name,
                ListSet("c")
              )
            )
          )
        )
      }
    }

    "raise error if definition exists and but is not a record" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Record.Tuple1
        )
      val addEnumValues = input.Action.AddEnumValues(inputs.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(
                inputs.Domain1Ref.name,
                inputs.Tuple1Ref.name,
                "enum",
                outputs.Data.Definition.Record.Tuple1
              )
            )
          )
        )
      }
    }

    "raise error if enum doesn't exist" in new Fixture {
      val snapshot      = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val addEnumValues = input.Action.AddEnumValues(inputs.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name)
            )
          )
        )
      }
    }
  }

  "ActionCompiler on RemoveEnumValues" should {

    "remove values if enum exists and all of the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val removeEnumValues = input.Action.RemoveEnumValues(inputs.Enum1Ref, ListSet("a"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        val internalRef = definitionRefIso.get(removeEnumValues.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Enum1.withValues(ListSet("b"))
      }
    }

    "raise error if enum exists and some the values are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val removeEnumValues = input.Action.RemoveEnumValues(inputs.Enum1Ref, ListSet("a", "c"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.EnumValuesMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name, ListSet("c")))
          )
        )
      }
    }

    "raise error if definition exists and but is not an enum" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Record.Tuple1
        )
      val removeEnumValues = input.Action.RemoveEnumValues(inputs.Enum1Ref, ListSet("a", "c"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(inputs.Domain1Ref.name,
                                                 inputs.Tuple1Ref.name,
                                                 "enum",
                                                 outputs.Data.Definition.Record.Tuple1)
            )
          )
        )
      }
    }

    "raise error if enum doesn't exist" in new Fixture {
      val snapshot         = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val removeEnumValues = input.Action.RemoveEnumValues(inputs.Enum1Ref, ListSet("a"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RenameEnumValues" should {

    "rename values if enum exists and all of the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val renameEnumValues = input.Action.RenameEnumValues(inputs.Enum1Ref, ListMap("a" -> "c"))

      new TestSnapshot(ActionCompiler(renameEnumValues))(snapshot) {
        val internalRef = definitionRefIso.get(renameEnumValues.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Enum1.withValues(ListSet("c", "b"))
      }
    }

    "raise error if enum exists and some the values are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val renameEnumValues = input.Action.RenameEnumValues(inputs.Enum1Ref, ListMap("c" -> "d"))

      new TestSnapshot(ActionCompiler(renameEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.EnumValuesMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name, ListSet("c")))
          )
        )
      }
    }

    "raise error if definition exists and but is not an enum" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Enum1Ref,
          inputs.Enum1Ref.name,
          outputs.Data.Definition.Record.Tuple1
        )
      val renameEnumValues = input.Action.RenameEnumValues(inputs.Enum1Ref, ListMap("a" -> "c"))

      new TestSnapshot(ActionCompiler(renameEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(inputs.Domain1Ref.name,
                                                 inputs.Tuple1Ref.name,
                                                 "enum",
                                                 outputs.Data.Definition.Record.Tuple1)
            )
          )
        )
      }
    }

    "raise error if enum doesn't exist" in new Fixture {
      val snapshot         = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val renameEnumValues = input.Action.RenameEnumValues(inputs.Enum1Ref, ListMap("a" -> "c"))

      new TestSnapshot(ActionCompiler(renameEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on AddRecordField" should {

    "add fields if records exists and none of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int))
        )
      val addRecordFields = input.Action.AddRecordFields(inputs.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        val internalRef = definitionRefIso.get(addRecordFields.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Tuple1
          .withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
      }
    }

    "raise error if records exists and some of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int))
        )
      val addRecordFields =
        input.Action.AddRecordFields(inputs.Tuple1Ref, ListMap("a" -> input.Data.Int, "b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.RecordFieldsExist(inputs.Domain1Ref.name, inputs.Tuple1Ref.name, ListSet("a")))
          )
        )
      }
    }

    "raise error if definition exists and but is not a record" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val addRecordFields = input.Action.AddRecordFields(inputs.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(inputs.Domain1Ref.name,
                                                 inputs.Tuple1Ref.name,
                                                 "record",
                                                 outputs.Data.Definition.Enum1)
            )
          )
        )
      }
    }

    "raise error if record doesn't exist" in new Fixture {
      val snapshot        = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)
      val addRecordFields = input.Action.AddRecordFields(inputs.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Tuple1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RemoveRecordFields" should {

    "remove fields if records exists and all of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val removeRecordFields = input.Action.RemoveRecordFields(inputs.Tuple1Ref, ListSet("b"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        val internalRef = definitionRefIso.get(removeRecordFields.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Tuple1
          .withFields(ListMap("a" -> output.Data.Int))
      }
    }

    "raise error if records exists and some of the fields are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val removeRecordFields = input.Action.RemoveRecordFields(inputs.Tuple1Ref, ListSet("c"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.RecordFieldsMissing(inputs.Domain1Ref.name, inputs.Tuple1Ref.name, ListSet("c"))
            )
          )
        )
      }
    }

    "raise error if definition exists and but is not a record" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val removeRecordFields = input.Action.RemoveRecordFields(inputs.Tuple1Ref, ListSet("c"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(inputs.Domain1Ref.name,
                                                 inputs.Tuple1Ref.name,
                                                 "record",
                                                 outputs.Data.Definition.Enum1)
            )
          )
        )
      }
    }

    "raise error if record doesn't exist" in new Fixture {
      val snapshot = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)

      val removeRecordFields = input.Action.RemoveRecordFields(inputs.Tuple1Ref, ListSet("b"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Tuple1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RenameRecordFields" should {

    "rename fields if records exists and all of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val renameRecordFields = input.Action.RenameRecordFields(inputs.Tuple1Ref, ListMap("a" -> "c"))

      new TestSnapshot(ActionCompiler(renameRecordFields))(snapshot) {
        val internalRef = definitionRefIso.get(renameRecordFields.definition)
        snapshot.definitions(internalRef) === outputs.Data.Definition.Record.Tuple1
          .withFields(ListMap("c" -> output.Data.Int, "b" -> output.Data.Double))
      }
    }

    "raise error if records exists and some of the fields are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val renameRecordFields = input.Action.RenameRecordFields(inputs.Tuple1Ref, ListMap("c" -> "d"))

      new TestSnapshot(ActionCompiler(renameRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.RecordFieldsMissing(inputs.Domain1Ref.name, inputs.Tuple1Ref.name, ListSet("c"))
            )
          )
        )
      }
    }

    "raise error if definition exists and but is not a record" in new Fixture {
      val snapshot = output
        .Snapshot()
        .withDefinition(
          outputs.Domain1Ref,
          inputs.Domain1Ref.name,
          outputs.Tuple1Ref,
          inputs.Tuple1Ref.name,
          outputs.Data.Definition.Enum1
        )
      val renameRecordFields = input.Action.RenameRecordFields(inputs.Tuple1Ref, ListMap("a" -> "e"))

      new TestSnapshot(ActionCompiler(renameRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(inputs.Domain1Ref.name,
                                                 inputs.Tuple1Ref.name,
                                                 "record",
                                                 outputs.Data.Definition.Enum1)
            )
          )
        )
      }
    }

    "raise error if record doesn't exist" in new Fixture {
      val snapshot = output.Snapshot().withDomainRef(outputs.Domain1Ref, inputs.Domain1Ref.name)

      val renameRecordFields = input.Action.RenameRecordFields(inputs.Tuple1Ref, ListMap("a" -> "e"))

      new TestSnapshot(ActionCompiler(renameRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionMissing(inputs.Domain1Ref.name, inputs.Tuple1Ref.name))
          )
        )
      }
    }
  }

  private trait Fixture extends Scope {
    val ActionCompiler = Provider.get[ActionCompiler[F]]
  }
}
