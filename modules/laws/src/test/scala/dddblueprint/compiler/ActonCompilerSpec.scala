package dddblueprint
package compiler

import io.scalaland.pulp.Provider
import monocle.macros.syntax.lens._
import org.specs2.specification.Scope

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
  }

  private trait Fixture extends Scope {
    val ActionCompiler = Provider.get[ActionCompiler[F]]
  }
}
