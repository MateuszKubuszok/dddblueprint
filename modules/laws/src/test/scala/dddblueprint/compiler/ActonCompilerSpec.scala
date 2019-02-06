package dddblueprint
package compiler

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

    // TODO: in future ensure tuples are only used as arguments
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

  private trait Fixture extends Scope {
    val SnapshotOperations = new SnapshotOperations[F]
    val ActionCompiler     = new ActionCompiler[F](SnapshotOperations)
  }
}
