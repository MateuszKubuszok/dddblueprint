package dddblueprint
package compiler

import cats.implicits._
import cats.mtl.implicits._
import cats.Monad
import io.scalaland.chimney.dsl._
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

@Cached final class SnapshotOperations[StateIO[_]: Monad: SchemaErrorRaise: SnapshotState] {

  def getDomainRef(name: output.DomainName): StateIO[Option[output.DomainRef]] =
    SnapshotState[StateIO].inspect { _.namespaces.domains.find(_._2 === name).map(_._1) }
  def hasDomainName(name: output.DomainName): StateIO[Boolean] =
    SnapshotState[StateIO].inspect { _.namespaces.domains.exists(_._2 === name) }
  def withDomainName(name: output.DomainName): StateIO[output.DomainRef] =
    getDomainRef(name).flatMap {
      case Some(ref) =>
        ref.pure[StateIO]
      case None =>
        for {
          oldVersion <- SnapshotState[StateIO].get
          ref = output.DomainRef()
          _ <- oldVersion.lens(_.namespaces.domains).modify(_ + (ref -> name)).set[StateIO]
        } yield ref
    }

  def getDefinitionRef(name: output.DefinitionName): StateIO[Option[output.DefinitionRef]] =
    SnapshotState[StateIO].inspect { _.namespaces.definitions.find(_._2 === name).map(_._1) }
  def hasDefinitionName(name: output.DefinitionName): StateIO[Boolean] =
    SnapshotState[StateIO].inspect { _.namespaces.definitions.exists(_._2 === name) }
  def withDefinitionName(name: output.DefinitionName): StateIO[output.DefinitionRef] =
    getDefinitionRef(name).flatMap {
      case Some(ref) => ref.pure[StateIO]
      case None =>
        for {
          oldVersion <- SnapshotState[StateIO].get
          ref = output.DefinitionRef()
          _ <- oldVersion.lens(_.namespaces.definitions).modify(_ + (ref -> name)).set[StateIO]
        } yield ref
    }

  def definitionToDomain(ref: output.DefinitionRef): StateIO[output.DomainRef] =
    SnapshotState[StateIO].get.flatMap {
      _.namespaces.definitions.get(ref) match {
        case Some(output.DefinitionName(domainRef, _)) => domainRef.pure[StateIO]
        case None                                      => SchemaError.invalidRef[StateIO, output.DomainRef](ref.id)
      }
    }

  def getDefinition(ref: output.DefinitionRef): StateIO[Option[output.Data.Definition]] =
    for {
      domainRef <- definitionToDomain(ref)
      body <- SnapshotState[StateIO].inspect {
        _.domains.get(domainRef).flatMap(_.definitions.get(ref))
      }
    } yield body
  def hasDefinition(ref: output.DefinitionRef): StateIO[Boolean] =
    getDefinition(ref).map(_.isDefined)
  def setDefinition(ref: output.DefinitionRef, body: output.Data.Definition): StateIO[Unit] =
    for {
      oldVersion <- SnapshotState[StateIO].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[StateIO]
        case None       => SchemaError.invalidRef[StateIO, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.withDefinition(name.domain, ref, body).set[StateIO]
    } yield ()
  def removeDefinition(ref: output.DefinitionRef): StateIO[Unit] =
    for {
      oldVersion <- SnapshotState[StateIO].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[StateIO]
        case None       => SchemaError.invalidRef[StateIO, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.withoutDefinition(name.domain, ref).set[StateIO]
    } yield ()
  def renameDefinition(ref: output.DefinitionRef, rename: String): StateIO[Unit] =
    for {
      oldVersion <- SnapshotState[StateIO].get
      name <- oldVersion.namespaces.definitions.get(ref) match {
        case Some(name) => name.pure[StateIO]
        case None       => SchemaError.invalidRef[StateIO, output.DefinitionName](ref.id)
      }
      _ <- oldVersion.namespaces.definitions.values.find { case output.DefinitionName(_, n) => n === rename } match {
        case Some(output.DefinitionName(domain, _)) =>
          SchemaError.definitionExists[StateIO, Unit](oldVersion.namespaces.domains(domain).name, rename)
        case None => ().pure[StateIO]
      }
      _ <- oldVersion.renameDefinition(name.domain, ref, rename).set[StateIO]
    } yield ()

  def translateDomainRef(ref: input.DomainRef): StateIO[output.DomainName] =
    ref.transformInto[output.DomainName].pure[StateIO]
  def translateDefinitionRef(ref: input.DefinitionRef): StateIO[output.DefinitionRef] =
    for {
      domainName <- translateDomainRef(ref.domain)
      domainRef <- withDomainName(domainName)
      definitionRef <- withDefinitionName(
        ref.into[output.DefinitionName].withFieldConst(_.domain, domainRef).transform
      )
    } yield definitionRef

  def requireDefinitionExists(ref: input.DefinitionRef): StateIO[output.DefinitionRef] =
    for {
      internalRef <- translateDefinitionRef(ref)
      definitionExists <- hasDefinition(internalRef)
      _ <- if (!definitionExists)
        SchemaError.definitionMissing[StateIO, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[StateIO]
    } yield internalRef

  def requireDefinitionNotExisted(ref: input.DefinitionRef): StateIO[output.DefinitionRef] =
    for {
      internalRef <- translateDefinitionRef(ref)
      definitionExists <- hasDefinition(internalRef)
      _ <- if (definitionExists) SchemaError.definitionExists[StateIO, Unit](domain = ref.domain.name, name = ref.name)
      else ().pure[StateIO]
    } yield internalRef
}

object SnapshotOperations {

  @inline def apply[StateIO[_]](implicit snapshotOperations: SnapshotOperations[StateIO]): SnapshotOperations[StateIO] =
    snapshotOperations
}
