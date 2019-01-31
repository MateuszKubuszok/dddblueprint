package dddblueprint
package schema

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class DefinitionRef(domain: DomainRef, name: String)
