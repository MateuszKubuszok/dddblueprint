package dddblueprint
package compiler

import java.util.UUID

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.ShowPretty
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
                                          actual:   output.Data.Definition)
      extends SchemaError

  final case class EnumValuesExist(domain:   String, name: String, values:  ListSet[String]) extends SchemaError
  final case class EnumValuesMissing(domain: String, name: String, values:  ListSet[String]) extends SchemaError
  final case class EnumTypeMismatch(domain:  String, name: String, oldType: String, newType: String) extends SchemaError

  final case class RecordFieldExists(domain:  String, name: String, values: ListSet[String]) extends SchemaError
  final case class RecordFieldMissing(domain: String, name: String, values: ListSet[String]) extends SchemaError

  final case class FieldDefinitionMissing(domain:    String, name: String, field:    String) extends SchemaError
  final case class ArgumentDefinitionMissing(domain: String, name: String, argument: String) extends SchemaError

  def invalidRef[F[_]: SchemaErrorRaise, A](ref: UUID): F[A] =
    NonEmptyList.one(InvalidRef(ref): SchemaError).raise[F, A]

  def domainMissing[F[_]: SchemaErrorRaise, A](domain: String): F[A] =
    NonEmptyList.one(DomainMissing(domain): SchemaError).raise[F, A]

  def definitionExists[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    NonEmptyList.one(DefinitionExists(domain, name): SchemaError).raise[F, A]
  def definitionMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    NonEmptyList.one(DefinitionMissing(domain, name): SchemaError).raise[F, A]
  def definitionTypeMismatch[F[_]: SchemaErrorRaise, A](domain: String,
                                                        name:     String,
                                                        expected: String,
                                                        actual:   output.Data.Definition): F[A] =
    NonEmptyList.one(DefinitionTypeMismatch(domain, name, expected, actual): SchemaError).raise[F, A]

  def enumValuesExist[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    NonEmptyList.one(EnumValuesExist(domain, name, values): SchemaError).raise[F, A]
  def enumValuesMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    NonEmptyList.one(EnumValuesMissing(domain, name, values): SchemaError).raise[F, A]
  def enumTypeMismatch[F[_]: SchemaErrorRaise, A](domain: String,
                                                  name:    String,
                                                  oldType: String,
                                                  newType: String): F[A] =
    NonEmptyList.one(EnumTypeMismatch(domain, name, oldType, newType): SchemaError).raise[F, A]

  def recordFieldsExist[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    NonEmptyList.one(RecordFieldExists(domain, name, values): SchemaError).raise[F, A]
  def recordFieldsMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, values: ListSet[String]): F[A] =
    NonEmptyList.one(RecordFieldMissing(domain, name, values): SchemaError).raise[F, A]

  def fieldDefinitionMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, field: String): F[A] =
    NonEmptyList.one(FieldDefinitionMissing(domain, name, field): SchemaError).raise[F, A]
  def argumentDefinitionMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String, argument: String): F[A] =
    NonEmptyList.one(ArgumentDefinitionMissing(domain, name, argument): SchemaError).raise[F, A]
}
