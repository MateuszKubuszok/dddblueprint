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
      val definition       = input.Fixtures.Data.Definition.Enum1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Enum1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile tuple definition" in new Fixture {
      val definition       = input.Fixtures.Data.Definition.Record.Tuple1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Tuple1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile entity definition" in new Fixture {
      val definition       = input.Fixtures.Data.Definition.Record.Entity1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Entity1
          .lens(_.ref)
          .set(internalRef)
      }
    }

    "correctly compile value definition" in new Fixture {
      val definition       = input.Fixtures.Data.Definition.Record.Value1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Value1.lens(_.ref).set(internalRef)
      }
    }

    "correctly compile event definition" in new Fixture {
      val definition       = input.Fixtures.Data.Definition.Record.Event1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Event1.lens(_.ref).set(internalRef)
      }
    }

    "raise error if definition already exists" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Enum1Ref, output.Fixtures.Data.Definition.Enum1)
      val definition       = input.Fixtures.Data.Definition.Enum1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(SchemaError.DefinitionExists(input.Fixtures.Domain1Ref.name, input.Fixtures.Enum1Ref.name))
          )
        )
      }
    }
  }

  "ActionCompiler on RemoveDefinition" should {

    "correctly remove definition" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Enum1Ref, output.Fixtures.Data.Definition.Enum1)
      val removeDefinition = input.Action.RemoveDefinition(input.Fixtures.Enum1Ref)

      new TestSnapshot(ActionCompiler(removeDefinition))(snapshot) {
        val internalRef = definitionRefIso.get(removeDefinition.definition)
        snapshot.definitions.get(internalRef) === None
      }
    }

    "raise error if definition doesn't exist" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
      val removeDefinition = input.Action.RemoveDefinition(input.Fixtures.Enum1Ref)

      new TestSnapshot(ActionCompiler(removeDefinition))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(input.Fixtures.Domain1Ref.name, input.Fixtures.Enum1Ref.name)
            )
          )
        )
      }
    }
  }

  "ActionCompiler on AddEnumValues" should {

    "add values if enum exists and none of the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Enum1Ref, output.Fixtures.Data.Definition.Enum1)
      val addEnumValues = input.Action.AddEnumValues(input.Fixtures.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        val internalRef = definitionRefIso.get(addEnumValues.definition)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Enum1
          .withValues(ListSet("a", "b", "c", "d"))
      }
    }

    "raise error if enum exists and some the values existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(
          output.Fixtures.Domain1Ref,
          output.Fixtures.Enum1Ref,
          output.Fixtures.Data.Definition.Enum1.withValues(ListSet("a", "b", "c"))
        )
      val addEnumValues = input.Action.AddEnumValues(input.Fixtures.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.EnumValuesExist(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Enum1Ref.name,
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
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref,
                        output.Fixtures.Enum1Ref,
                        output.Fixtures.Data.Definition.Record.Tuple1)
      val addEnumValues = input.Action.AddEnumValues(input.Fixtures.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
                "enum",
                output.Fixtures.Data.Definition.Record.Tuple1
              )
            )
          )
        )
      }
    }

    "raise error if enum doesn't exist" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
      val addEnumValues = input.Action.AddEnumValues(input.Fixtures.Enum1Ref, ListSet("c", "d"))

      new TestSnapshot(ActionCompiler(addEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(input.Fixtures.Domain1Ref.name, input.Fixtures.Enum1Ref.name)
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
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Enum1Ref, output.Fixtures.Data.Definition.Enum1)
      val removeEnumValues = input.Action.RemoveEnumValues(input.Fixtures.Enum1Ref, ListSet("a"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        val internalRef = definitionRefIso.get(removeEnumValues.definition)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Enum1.withValues(ListSet("b"))
      }
    }

    "raise error if enum exists and some the values are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Enum1Ref, output.Fixtures.Data.Definition.Enum1)
      val removeEnumValues = input.Action.RemoveEnumValues(input.Fixtures.Enum1Ref, ListSet("a", "c"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.EnumValuesMissing(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Enum1Ref.name,
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
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Enum1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                 input.Fixtures.Enum1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref,
                        output.Fixtures.Enum1Ref,
                        output.Fixtures.Data.Definition.Record.Tuple1)
      val removeEnumValues = input.Action.RemoveEnumValues(input.Fixtures.Enum1Ref, ListSet("a", "c"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
                "enum",
                output.Fixtures.Data.Definition.Record.Tuple1
              )
            )
          )
        )
      }
    }

    "raise error if enum doesn't exist" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
      val removeEnumValues = input.Action.RemoveEnumValues(input.Fixtures.Enum1Ref, ListSet("a"))

      new TestSnapshot(ActionCompiler(removeEnumValues))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(input.Fixtures.Domain1Ref.name, input.Fixtures.Enum1Ref.name)
            )
          )
        )
      }
    }
  }

  "ActionCompiler on AddRecordField" should {

    "add fields if records exists and none of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(
          output.Fixtures.Domain1Ref,
          output.Fixtures.Tuple1Ref,
          output.Fixtures.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int))
        )
      val addRecordFields = input.Action.AddRecordFields(input.Fixtures.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        val internalRef = definitionRefIso.get(addRecordFields.definition)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Tuple1
          .withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
      }
    }

    "raise error if records exists and some of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(
          output.Fixtures.Domain1Ref,
          output.Fixtures.Tuple1Ref,
          output.Fixtures.Data.Definition.Record.Tuple1.withFields(ListMap("a" -> output.Data.Int))
        )
      val addRecordFields =
        input.Action.AddRecordFields(input.Fixtures.Tuple1Ref, ListMap("a" -> input.Data.Int, "b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.RecordFieldsExist(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
                ListSet("a")
              )
            )
          )
        )
      }
    }

    "raise error if definition exists and but is not a record" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Tuple1Ref, output.Fixtures.Data.Definition.Enum1)
      val addRecordFields = input.Action.AddRecordFields(input.Fixtures.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
                "record",
                output.Fixtures.Data.Definition.Enum1
              )
            )
          )
        )
      }
    }

    "raise error if record doesn't exist" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
      val addRecordFields = input.Action.AddRecordFields(input.Fixtures.Tuple1Ref, ListMap("b" -> input.Data.Double))

      new TestSnapshot(ActionCompiler(addRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(input.Fixtures.Domain1Ref.name, input.Fixtures.Tuple1Ref.name)
            )
          )
        )
      }
    }
  }

  "ActionCompiler on RemoveRecordFields" should {

    "remove fields if records exists and all of the fields existed before" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(
          output.Fixtures.Domain1Ref,
          output.Fixtures.Tuple1Ref,
          output.Fixtures.Data.Definition.Record.Tuple1
            .withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val removeRecordFields = input.Action.RemoveRecordFields(input.Fixtures.Tuple1Ref, ListSet("b"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        val internalRef = definitionRefIso.get(removeRecordFields.definition)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Record.Tuple1
          .withFields(ListMap("a" -> output.Data.Int))
      }
    }

    "raise error if records exists and some of the fields are missing" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(
          output.Fixtures.Domain1Ref,
          output.Fixtures.Tuple1Ref,
          output.Fixtures.Data.Definition.Record.Tuple1
            .withFields(ListMap("a" -> output.Data.Int, "b" -> output.Data.Double))
        )
      val removeRecordFields = input.Action.RemoveRecordFields(input.Fixtures.Tuple1Ref, ListSet("c"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.RecordFieldsMissing(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
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
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
        .lens(_.namespaces.definitions)
        .modify(
          _ + (output.Fixtures.Tuple1Ref -> output.DefinitionName(output.Fixtures.Domain1Ref,
                                                                  input.Fixtures.Tuple1Ref.name))
        )
        .withDefinition(output.Fixtures.Domain1Ref, output.Fixtures.Tuple1Ref, output.Fixtures.Data.Definition.Enum1)
      val removeRecordFields = input.Action.RemoveRecordFields(input.Fixtures.Tuple1Ref, ListSet("c"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(
                input.Fixtures.Domain1Ref.name,
                input.Fixtures.Tuple1Ref.name,
                "record",
                output.Fixtures.Data.Definition.Enum1
              )
            )
          )
        )
      }
    }

    "raise error if record doesn't exist" in new Fixture {
      val snapshot = output
        .Snapshot()
        .lens(_.namespaces.domains)
        .modify(_ + (output.Fixtures.Domain1Ref -> output.DomainName(input.Fixtures.Domain1Ref.name)))
      val removeRecordFields = input.Action.RemoveRecordFields(input.Fixtures.Tuple1Ref, ListSet("b"))

      new TestSnapshot(ActionCompiler(removeRecordFields))(snapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionMissing(input.Fixtures.Domain1Ref.name, input.Fixtures.Tuple1Ref.name)
            )
          )
        )
      }
    }
  }

  private trait Fixture extends Scope {
    val ActionCompiler = Provider.get[ActionCompiler[F]]
  }
}
