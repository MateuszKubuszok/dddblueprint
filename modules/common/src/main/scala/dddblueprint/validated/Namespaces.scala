package dddblueprint
package validated

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.ListMap

@Semi(Eq, Show) final case class Namespaces(domains:     ListMap[DomainRef, DomainName]         = ListMap.empty,
                                            definitions: ListMap[DefinitionRef, DefinitionName] = ListMap.empty)
