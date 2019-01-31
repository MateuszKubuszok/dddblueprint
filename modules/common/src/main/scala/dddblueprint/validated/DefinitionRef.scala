package dddblueprint
package validated

import java.util.UUID

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class DefinitionRef(id: UUID = UUID.randomUUID)
