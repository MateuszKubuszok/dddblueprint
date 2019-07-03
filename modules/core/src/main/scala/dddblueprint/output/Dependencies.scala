package dddblueprint
package output

import cats.Eq
import cats.implicits._
import cats.derived.ShowPretty
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class Dependencies(direct:     ListSet[DefinitionRef],
                                                    transitive: ListSet[DefinitionRef]) {

  assert(direct.subsetOf(transitive), "Transitive dependencies must include direct dependencies")
}

object Dependencies {

  def apply(direct: ListSet[DefinitionRef]): Dependencies =
    new Dependencies(direct, direct)
}
