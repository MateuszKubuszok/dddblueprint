package dddblueprint
package compiler

import cats.implicits._
import cats.mtl.implicits._
import cats.Monad
import io.scalaland.chimney.dsl._
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached class SnapshotOperations[F[_]: Monad: SchemaErrorRaise: SnapshotState] {

  def getDomainRef(name: output.DomainName): F[Option[output.DomainRef]] =
    SnapshotState[F].inspect { _.namespaces.domains.find(_._2 === name).map(_._1) }
  def hasDomainName(name: output.DomainName): F[Boolean] =
    SnapshotState[F].inspect { _.namespaces.domains.exists(_._2 === name) }
  def withDomainName(name: output.DomainName): F[output.DomainRef] =
    getDomainRef(name).flatMap {
      case Some(ref) =>
        ref.pure[F]
      case None =>
        for {
          oldVersion <- SnapshotState[F].get
          ref = output.DomainRef()
          _ <- oldVersion.lens(_.namespaces.domains).modify(_ + (ref -> name)).set[F]
        } yield ref
    }

  def getDefinitionRef(name: output.DefinitionName): F[Option[output.DefinitionRef]] =
    SnapshotState[F].inspect { _.namespaces.definitions.find(_._2 === name).map(_._1) }
  def hasDefinitionName(name: output.DefinitionName): F[Boolean] =
    SnapshotState[F].inspect { _.namespaces.definitions.exists(_._2 === name) }
  def withDefinitionName(name: output.DefinitionName): F[output.DefinitionRef] =
    getDefinitionRef(name).flatMap {
      case Some(ref) => ref.pure[F]
      case None =>
        for {
          oldVersion <- SnapshotState[F].get
          ref = output.DefinitionRef()
          _ <- oldVersion.lens(_.namespaces.definitions).modify(_ + (ref -> name)).set[F]
        } yield ref
    }

  def definitionToDomain(ref: output.DefinitionRef): F[output.DomainRef] =
    SnapshotState[F].get.flatMap {
      _.namespaces.definitions.get(ref) match {
        case Some(output.DefinitionName(domainRef, _)) => domainRef.pure[F]
        case None                                      => SchemaError.invalidRef[F, output.DomainRef](ref.id)
      }
    }

  def getDefinition(ref: output.DefinitionRef): F[Option[output.Data.Definition]] =
    for {
      domainRef <- definitionToDomain(ref)
      body <- SnapshotState[F].inspect {
        _.domains.get(domainRef).flatMap(_.definitions.get(ref))
      }
    } yield body
  def hasDefinition(ref: output.DefinitionRef): F[Boolean] =
    getDefinition(ref).map(_.isDefined)
  def setDefinition(ref: output.DefinitionRef, body: output.Data.Definition): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[F]
        case None       => SchemaError.invalidRef[F, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.withDefinition(name.domain, ref, body).set[F]
    } yield ()
  def removeDefinition(ref: output.DefinitionRef): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[F]
        case None       => SchemaError.invalidRef[F, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.withoutDefinition(name.domain, ref).set[F]
    } yield ()
  def renameDefinition(ref: output.DefinitionRef, rename: String): F[Unit] =
    for {
      oldVersion <- SnapshotState[F].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[F]
        case None       => SchemaError.invalidRef[F, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.namespaces.definitions.values.find { case output.DefinitionName(_, n) => n === rename } match {
        case Some(output.DefinitionName(domain, _)) =>
          SchemaError.definitionExists[F, Unit](oldVersion.namespaces.domains(domain).name, rename)
        case None => ().pure[F]
      }
      _ <- oldVersion.renameDefinition(name.domain, ref, rename).set[F]
    } yield ()

  def translateDomainRef(ref: input.DomainRef): F[output.DomainName] =
    ref.transformInto[output.DomainName].pure[F]
  def translateDefinitionRef(ref: input.DefinitionRef): F[output.DefinitionRef] =
    for {
      domainName <- translateDomainRef(ref.domain)
      domainRef <- withDomainName(domainName)
      definitionRef <- withDefinitionName(
        ref.into[output.DefinitionName].withFieldConst(_.domain, domainRef).transform
      )
    } yield definitionRef

  def requireDefinitionExists(ref: input.DefinitionRef): F[output.DefinitionRef] =
    for {
      internalRef <- translateDefinitionRef(ref)
      definitionExists <- hasDefinition(internalRef)
      _ <- if (!definitionExists) SchemaError.definitionMissing[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef

  def requireDefinitionNotExisted(ref: input.DefinitionRef): F[output.DefinitionRef] =
    for {
      internalRef <- translateDefinitionRef(ref)
      definitionExists <- hasDefinition(internalRef)
      _ <- if (definitionExists) SchemaError.definitionExists[F, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[F]
    } yield internalRef
}

object SnapshotOperations {

  @inline def apply[F[_]](implicit snapshotOperations: SnapshotOperations[F]): SnapshotOperations[F] =
    snapshotOperations
}
