package dddblueprint
package validated

import java.util.UUID

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) final case class DomainRef(id: UUID = UUID.randomUUID)