package dddblueprint
package validated

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.ListMap

@Semi(Eq, ShowPretty) final case class Snapshot(
  namespaces: Namespaces                      = Namespaces(),
  domains:    ListMap[DomainRef, Definitions] = ListMap.empty,
  version:    Int                             = 0
) {

  def bumpVersion: Snapshot = copy(version = version + 1)
}
