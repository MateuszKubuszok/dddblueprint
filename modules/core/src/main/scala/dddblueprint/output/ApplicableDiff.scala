package dddblueprint
package output

import cats.derived.ShowPretty
import cats.Eq
import cats.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, ShowPretty) sealed trait ApplicableDiff extends ADT
object ApplicableDiff {
  final case class DefinitionRenamed(to:  String) extends ApplicableDiff
  final case class FieldRenamed(from:     String, to: String) extends ApplicableDiff
  final case class EnumValueRenamed(from: String, to: String) extends ApplicableDiff
}
