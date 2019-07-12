package dddblueprint
package output

import cats.derived.ShowPretty
import cats.Eq
import cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) sealed trait ManualDiff extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object ManualDiff {
  final case object DefinitionRemoved extends ManualDiff
  final case class EnumValueRemoved(name:   String) extends ManualDiff
  final case class RecordFieldRemoved(name: String) extends ManualDiff
}
