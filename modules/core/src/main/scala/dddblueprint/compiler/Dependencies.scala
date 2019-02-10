package dddblueprint
package compiler

import dddblueprint.output.DefinitionRef

import scala.collection.immutable.ListSet

final case class Dependencies(direct: ListSet[output.DefinitionRef], transitive: ListSet[output.DefinitionRef]) {

  assert(direct.subsetOf(transitive), "Transitive dependencies must include direct dependencies")
}

object Dependencies {

  def apply(direct: ListSet[DefinitionRef]): Dependencies =
    new Dependencies(direct, direct)
}
