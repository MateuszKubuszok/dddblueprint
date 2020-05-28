package dddblueprint
package output

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class Namespaces(
  domains:     ListMap[DomainRef, DomainName]         = ListMap.empty,
  definitions: ListMap[DefinitionRef, DefinitionName] = ListMap.empty,
  versions:    ListMap[DefinitionRef, Int]            = ListMap.empty
)
