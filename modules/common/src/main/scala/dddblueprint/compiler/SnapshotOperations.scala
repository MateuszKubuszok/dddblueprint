package dddblueprint
package compiler

import cats.implicits._
import cats.mtl.implicits._
import cats.Monad
import io.scalaland.chimney.dsl._
import monocle.macros.syntax.lens._

import scala.collection.immutable.ListMap

class SnapshotOperations[F[_]: Monad: SchemaErrorRaise: SnapshotState] {

  def getDomainRef(name: validated.DomainName): F[Option[validated.DomainRef]] =
    SnapshotState[F].inspect { _.namespaces.domains.find(_._2 === name).map(_._1) }
  def hasDomainName(name: validated.DomainName): F[Boolean] =
    SnapshotState[F].inspect { _.namespaces.domains.exists(_._2 === name) }
  def withDomainName(name: validated.DomainName): F[validated.DomainRef] =
    getDomainRef(name).flatMap {
      case Some(ref) =>
        ref.pure[F]
      case None =>
        for {
          oldVersion <- SnapshotState[F].get
          ref = validated.DomainRef()
          _ <- oldVersion.lens(_.namespaces.domains).modify(_ + (ref -> name)).set[F]
        } yield ref
    }

  def getDefinitionRef(name: validated.DefinitionName): F[Option[validated.DefinitionRef]] =
    SnapshotState[F].inspect { _.namespaces.definitions.find(_._2 === name).map(_._1) }
  def hasDefinitionName(name: validated.DefinitionName): F[Boolean] =
    SnapshotState[F].inspect { _.namespaces.definitions.exists(_._2 === name) }
  def withDefinitionName(name: validated.DefinitionName): F[validated.DefinitionRef] =
    getDefinitionRef(name).flatMap {
      case Some(ref) => ref.pure[F]
      case None =>
        for {
          oldVersion <- SnapshotState[F].get
          ref = validated.DefinitionRef()
          _ <- oldVersion.lens(_.namespaces.definitions).modify(_ + (ref -> name)).set[F]
        } yield ref
    }

  def definitionToDomain(ref: validated.DefinitionRef): F[validated.DomainRef] =
    SnapshotState[F].get.flatMap {
      _.namespaces.definitions.get(ref) match {
        case Some(validated.DefinitionName(domainRef, _)) => domainRef.pure[F]
        case None                                         => SchemaError.invalidRef[F, validated.DomainRef](ref.id)
      }
    }

  def getDefinition(ref: validated.DefinitionRef): F[Option[validated.Data.Definition]] =
    for {
      domainRef <- definitionToDomain(ref)
      body <- SnapshotState[F].inspect {
        _.domains.get(domainRef).flatMap(_.definitions.get(ref))
      }
    } yield body
  def hasDefinition(ref: validated.DefinitionRef): F[Boolean] =
    getDefinition(ref).map(_.isDefined)
  def setDefinition(ref: validated.DefinitionRef, body: validated.Data.Definition): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[F]
        case None       => SchemaError.invalidRef[F, validated.DefinitionName](ref.id)
      }
      _ <- oldVersion
        .lens(_.domains)
        .modify { domains =>
          val newDefinitions = domains.get(name.domain) match {
            case Some(definitions) => definitions.lens(_.definitions).modify(_.updated(ref, body))
            case None              => validated.Definitions(definitions = ListMap(ref -> body))
          }
          domains.updated(name.domain, newDefinitions)
        }
        .set[F]
    } yield ()

  def translateDomainRef(ref: schema.DomainRef): F[validated.DomainName] =
    ref.transformInto[validated.DomainName].pure[F]
  def translateDefinitionRef(ref: schema.DefinitionRef): F[validated.DefinitionRef] =
    for {
      domainName <- translateDomainRef(ref.domain)
      domainRef <- withDomainName(domainName)
      definitionRef <- withDefinitionName(
        ref.into[validated.DefinitionName].withFieldConst(_.domain, domainRef).transform
      )
    } yield definitionRef
}
