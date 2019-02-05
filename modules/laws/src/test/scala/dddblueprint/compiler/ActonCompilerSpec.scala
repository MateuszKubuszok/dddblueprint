package dddblueprint
package compiler

import monocle.macros.syntax.lens._
import org.specs2.specification.Scope

class ActonCompilerSpec extends CompilerSpec {

  "ActionCompiler" should {

    "correctly compile CreateDefinition for enum" in new Fixture {
      val definition       = input.Fixtures.Data.Definition.Enum1
      val createDefinition = input.Action.CreateDefinition(definition)

      new TestSnapshot(ActionCompiler(createDefinition))() {
        val internalRef = definitionRefIso.get(definition.ref)
        snapshot.definitions(internalRef) === output.Fixtures.Data.Definition.Enum1.lens(_.ref).set(internalRef)
      }
    }
  }

  private trait Fixture extends Scope {
    val SnapshotOperations = new SnapshotOperations[F]
    val ActionCompiler     = new ActionCompiler[F](SnapshotOperations)
  }
}
