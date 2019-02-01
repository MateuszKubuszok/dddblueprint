package dddblueprint
package output

import cats.Eq
import cats.derived.ShowPretty
import cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

@Semi(Eq, ShowPretty) final case class Snapshot(
  namespaces:       Namespaces                      = Namespaces(),
  domains:          ListMap[DomainRef, Definitions] = ListMap.empty,
  version:          Int                             = 0,
  manualMigrations: ListSet[DefinitionRef]          = ListSet.empty
) {

  def bumpVersion: Snapshot = copy(version = version + 1, manualMigrations = ListSet.empty)
}
