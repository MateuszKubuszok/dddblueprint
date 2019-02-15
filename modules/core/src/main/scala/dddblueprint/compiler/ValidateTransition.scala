package dddblueprint
package compiler

import cats.Traverse
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import cats.mtl.implicits._
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

import scala.collection.immutable.{ ListMap, ListSet }

@Cached class ValidateTransition[F[_]: Sync: SnapshotState: SchemaErrorRaise] {

  // TODO: potentially throwing, fix later
  // use new
  // fallback on old
  // add some mock if not found
  private def refToDomainAndName(version: output.Snapshot, ref: output.DefinitionRef): (String, String) = {
    val output.DefinitionName(domainRef, name) = version.namespaces.definitions(ref)
    val output.DomainName(domainName)          = version.namespaces.domains(domainRef)
    domainName -> name
  }

  private def resolveDependencies(newVersion: output.Snapshot): F[ListMap[output.DefinitionRef, Dependencies]] =
    Sync[F].delay(DependencyResolver.findTransitiveDependencies(newVersion.definitions))

  private def findMissing(version:   output.Snapshot,
                          ref:       output.DefinitionRef,
                          isDefined: output.DefinitionRef => Boolean)(
    usedRefs:                        ListMap[String, output.DefinitionRef]
  ): List[SchemaError] = {
    val (domain, name) = refToDomainAndName(version, ref)
    val refNames       = usedRefs.map(_.swap)
    usedRefs.values.filterNot(isDefined).toList.map { refWithoutDef =>
      SchemaError.FieldDefinitionMissing(domain = domain, name = name, field = refNames(refWithoutDef)): SchemaError
    }
  }

  def apply(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] =
    // first - check if types matches and no definition is missing
    removedAreNotUsed(newVersion)
      .map2(typesMatches(oldVersion, newVersion))(_ |+| _)
      // then calculate dependencies
      .flatMap { _ =>
        resolveDependencies(newVersion)
      }
      // do normal checking
      .flatMap { dependencies =>
        pubsubDependsOnlyOnEvents(newVersion, dependencies)
          .map2(eventPublishedInTheirDomain(newVersion, dependencies))(_ |+| _)
          .map2(migrationsForEnums(oldVersion, newVersion))(_ |+| _)
          .map2(migrationsForRecords(oldVersion, newVersion, dependencies))(_ |+| _)
      }

  def removedAreNotUsed(newVersion: output.Snapshot): F[Unit] = Sync[F].defer {
    val isDefined = newVersion.definitions.keySet.contains _
    newVersion.definitions.flatMap {
      case (ref, body) =>
        findMissing(newVersion, ref, isDefined)(
          DependencyResolver.dataToDirectNamedDependencies(newVersion.findName)(body)
        )
    } match {
      case head :: tail => NonEmptyList(head, tail).raise[F, Unit]
      case Nil          => ().pure[F]
    }
  }

  // scalastyle:off cyclomatic.complexity
  def typesMatches(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = Sync[F].defer {
    (for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet]
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet]
      if oldRef === newRef
      (domain, name) = refToDomainAndName(newVersion, newRef)
      typeMismatch <- (oldDef, newDef) match {
        case (output.Data.Definition.Enum(_, _, oldType), output.Data.Definition.Enum(_, _, newType)) =>
          if (oldType =!= newType) {
            ListSet(SchemaError.EnumTypeMismatch(domain, name, oldType.show, newType.show): SchemaError)
          } else { ListSet.empty[SchemaError] }
        case (_: output.Data.Definition.Enum, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, "enum", other): SchemaError)

        case (_: output.Data.Definition.Record, _: output.Data.Definition.Record) =>
          ListSet.empty[SchemaError]
        case (record: output.Data.Definition.Record, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, record.`type`.show, other): SchemaError)

        case (_: output.Data.Definition.Service, _: output.Data.Definition.Service) =>
          ListSet.empty[SchemaError]
        case (_: output.Data.Definition.Service, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, "service", other): SchemaError)

        case (_: output.Data.Definition.Publisher, _: output.Data.Definition.Publisher) =>
          ListSet.empty[SchemaError]
        case (_: output.Data.Definition.Publisher, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, "publisher", other): SchemaError)

        case (_: output.Data.Definition.Subscriber, _: output.Data.Definition.Subscriber) =>
          ListSet.empty[SchemaError]
        case (_: output.Data.Definition.Subscriber, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, "subscriber", other): SchemaError)
      }
    } yield typeMismatch).toList match {
      case head :: tail => NonEmptyList[SchemaError](head, tail).raise[F, Unit]
      case Nil          => ().pure[F]
    }
  }
  // scalastyle:on

  def pubsubDependsOnlyOnEvents(newVersion:   output.Snapshot,
                                dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] = Sync[F].defer {
    dependencies.toList.flatMap {
      case (ref, Dependencies(direct, _)) =>
        for {
          _ <- newVersion.definitions.get(ref).toList.collect {
            case _: output.Data.Definition.Publisher  => ()
            case _: output.Data.Definition.Subscriber => ()
          }
          depsRef <- direct.toList
          isDepsEvent = newVersion.definitions.get(depsRef) match {
            case Some(_: output.Data.Definition.Record.Event) => true
            case _ => false
          }
          if !isDepsEvent
          (domain, name) = refToDomainAndName(newVersion, ref)
        } yield SchemaError.DefinitionTypeMismatch(domain, name, "event", newVersion.definitions(depsRef)): SchemaError
    } match {
      case head :: tail => NonEmptyList[SchemaError](head, tail).raise[F, Unit]
      case Nil          => ().pure[F]
    }
  }

  def eventPublishedInTheirDomain(newVersion:   output.Snapshot,
                                  dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] = Sync[F].defer {
    dependencies.toList.flatMap {
      case (ref, Dependencies(direct, _)) =>
        for {
          publisher <- newVersion.definitions.get(ref).toList.collect { case p: output.Data.Definition.Publisher => p }
          refDomain <- newVersion.findDomain(ref).toList
          (depsRef, depsDomain) <- direct.toList.flatMap(deps => newVersion.findDomain(deps).toList.map(deps -> _))
          isDepsEvent = newVersion.definitions.get(depsRef) match {
            case Some(_: output.Data.Definition.Record.Event) => true
            case _ => false
          }
          if isDepsEvent && refDomain =!= depsDomain
          (domain, name) = refToDomainAndName(newVersion, ref)
        } yield SchemaError.EventPublishedOutsideDomain(domain, name, publisher): SchemaError
    } match {
      case head :: tail => NonEmptyList[SchemaError](head, tail).raise[F, Unit]
      case Nil          => ().pure[F]
    }
  }

  def migrationsForEnums(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = Sync[F].defer {
    val enumsWithRemovedValues = for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      if oldRef === newRef && !oldDef.values.subsetOf(newDef.values)
    } yield newRef

    Traverse[ListSet]
      .sequence(
        DependencyResolver
          .findTransitiveDependencies(newVersion.definitions)
          .collect {
            case (ref, Dependencies(_, transitive)) if enumsWithRemovedValues.intersect(transitive).nonEmpty =>
              SnapshotState[F].modify { version =>
                version.lens(_.manualMigrations).modify { migrations =>
                  migrations
                    .updated(ref,
                             migrations.getOrElse(ref, ListSet.empty) ++ enumsWithRemovedValues.intersect(transitive))
                }
              }
          }
          .to[ListSet]
      )
      .map(_ => ())
  }

  def migrationsForRecords(oldVersion:   output.Snapshot,
                           newVersion:   output.Snapshot,
                           dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] = Sync[F].defer {
    val recordsWithChangedOrRemovedFields = for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Record) => ref -> enum
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Record) => ref -> enum
      }
      if oldRef === newRef
      fieldRemoved = !oldDef.fields.keySet.subsetOf(newDef.fields.keySet)
      fieldChanged = (for {
        (name1, arg1) <- oldDef.fields
        (name2, arg2) <- newDef.fields
        if name1 === name2
      } yield arg1.argumentType =!= arg2.argumentType).exists(identity)
      if fieldRemoved || fieldChanged
    } yield newRef

    val recordsRemoved = oldVersion.definitions.keySet -- newVersion.definitions.keySet

    val fieldOrRecordRemoved = recordsWithChangedOrRemovedFields ++ recordsRemoved

    Traverse[ListSet]
      .sequence(
        dependencies
          .collect {
            case (ref, Dependencies(_, transitive)) if fieldOrRecordRemoved.intersect(transitive).nonEmpty =>
              SnapshotState[F].modify { version =>
                version.lens(_.manualMigrations).modify { migrations =>
                  migrations
                    .updated(ref,
                             migrations.getOrElse(ref, ListSet.empty) ++ fieldOrRecordRemoved.intersect(transitive))
                }
              }
          }
          .to[ListSet]
      )
      .map(_ => ())
  }
}

object ValidateTransition {

  @inline def apply[F[_]](implicit validateTransition: ValidateTransition[F]): ValidateTransition[F] =
    validateTransition
}
