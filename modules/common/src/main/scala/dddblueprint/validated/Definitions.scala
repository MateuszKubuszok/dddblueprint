package dddblueprint
package validated

import scala.collection.immutable.ListMap

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class Definitions(definitions: ListMap[DefinitionRef, Data.Definition] = ListMap.empty)
