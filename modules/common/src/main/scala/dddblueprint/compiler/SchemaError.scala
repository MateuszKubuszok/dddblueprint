package dddblueprint
package compiler

import java.util.UUID

import cats.Eq, cats.derived.ShowPretty
import cats.implicits._
import cats.mtl.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.ListSet

@Semi(Eq, ShowPretty) sealed trait SchemaError extends ADT
object SchemaError {
  final case class InvalidRef(ref: UUID) extends SchemaError

  final case class DomainMissing(domain: String) extends SchemaError

  final case class DefinitionExists(domain:         String, name: String) extends SchemaError
  final case class DefinitionMissing(domain:        String, name: String) extends SchemaError
  final case class DefinitionTypeMismatch(domain:   String,
                                          name:     String,
                                          expected: String,
                                          actual:   validated.Data.Definition)
      extends SchemaError

  final case class EnumValuesExist(domain:   String, name: String, values: ListSet[String]) extends SchemaError
  final case class EnumValuesMissing(domain: String, name: String, values: ListSet[String]) extends SchemaError

  final case class RecordFieldExists(domain:  String, name: String, values: ListSet[String]) extends SchemaError
  final case class RecordFieldMissing(domain: String, name: String, values: ListSet[String]) extends SchemaError

  def invalidRef[F[_]: SchemaErrorRaise, A](ref: UUID): F[A] =
    (InvalidRef(ref): SchemaError).raise[F, A]

  def domainMissing[F[_]: SchemaErrorRaise, A](domain: String): F[A] =
    (DomainMissing(domain): SchemaError).raise[F, A]

  def definitionExists[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    (DefinitionExists(domain, name): SchemaError).raise[F, A]
  def definitionMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    (DefinitionMissing(domain, name): SchemaError).raise[F, A]
  def definitionTypeMismatch[F[_]: SchemaErrorRaise, A](domain: String,
                                                        name:     String,
                                                        expected: String,
                                                        actual:   validated.Data.Definition): F[A] =
    (DefinitionTypeMismatch(domain, name, expected, actual): SchemaError).raise[F, A]

  def enumValuesExist[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    (EnumValuesExist(domain, name, values): SchemaError).raise[F, A]
  def enumValuesMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    (EnumValuesMissing(domain, name, values): SchemaError).raise[F, A]

  def recordFieldsExist[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    (RecordFieldExists(domain, name, values): SchemaError).raise[F, A]
  def recordFieldsMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    (RecordFieldMissing(domain, name, values): SchemaError).raise[F, A]
}
