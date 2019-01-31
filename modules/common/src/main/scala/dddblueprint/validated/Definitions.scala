package dddblueprint
package validated

import scala.collection.immutable.ListMap

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) final case class Definitions(definitions: ListMap[DefinitionRef, Data.Definition] = ListMap.empty)
