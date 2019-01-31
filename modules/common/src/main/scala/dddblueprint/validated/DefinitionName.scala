package dddblueprint
package validated

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) final case class DefinitionName(domain: DomainRef, name: String)
