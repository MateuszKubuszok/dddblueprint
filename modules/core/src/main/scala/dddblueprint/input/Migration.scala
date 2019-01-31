package dddblueprint
package input

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class Migration(actions: List[Action]) extends ADT
