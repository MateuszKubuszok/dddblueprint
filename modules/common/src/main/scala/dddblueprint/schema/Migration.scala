package dddblueprint
package schema

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) final case class Migration(actions: List[Action]) extends ADT