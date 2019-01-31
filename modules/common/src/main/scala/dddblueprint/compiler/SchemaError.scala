package dddblueprint
package compiler

import java.util.UUID

import cats.{ Eq, Show }
import cats.implicits._
import cats.mtl.implicits._
import io.scalaland.catnip.Semi

@Semi(Eq, Show) sealed trait SchemaError extends ADT
object SchemaError {
  final case class InvalidRef(ref:           UUID) extends SchemaError
  final case class DomainMissing(domain:     String) extends SchemaError
  final case class DefinitionExists(domain:  String, name: String) extends SchemaError
  final case class DefinitionMissing(domain: String, name: String) extends SchemaError

  def invalidRef[F[_]: SchemaErrorRaise, A](ref: UUID): F[A] =
    (InvalidRef(ref): SchemaError).raise[F, A]
  def domainMissing[F[_]: SchemaErrorRaise, A](domain: String): F[A] =
    (DomainMissing(domain): SchemaError).raise[F, A]
  def definitionExists[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    (DefinitionExists(domain, name): SchemaError).raise[F, A]
  def definitionMissing[F[_]: SchemaErrorRaise, A](domain: String, name: String): F[A] =
    (DefinitionMissing(domain, name): SchemaError).raise[F, A]
}
