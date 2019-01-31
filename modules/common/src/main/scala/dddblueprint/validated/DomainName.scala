package dddblueprint
package validated

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) final case class DomainName(name: String)
