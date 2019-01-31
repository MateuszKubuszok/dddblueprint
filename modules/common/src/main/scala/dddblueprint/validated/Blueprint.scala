package dddblueprint
package validated

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) final case class Blueprint(versions: List[Snapshot] = List.empty)
