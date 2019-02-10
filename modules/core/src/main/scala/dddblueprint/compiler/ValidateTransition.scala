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

  def apply(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] =
    removedAreNotUsed(newVersion)
      .map2(typesMatches(oldVersion, newVersion))(_ |+| _)
      .map2(migrationsForEnums(oldVersion, newVersion))(_ |+| _)
      .map2(migrationsForRecords(oldVersion, newVersion))(_ |+| _)

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

  def removedAreNotUsed(newVersion: output.Snapshot): F[Unit] = Sync[F].defer {
    val isDefined = newVersion.definitions.keySet.contains _
    newVersion.definitions.flatMap {
      case (ref, body) =>
        findMissing(newVersion, ref, isDefined)(DependencyResolver.dataToDirectNamedDependencies(body))
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

  def migrationsForRecords(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = Sync[F].defer {
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
        DependencyResolver
          .findTransitiveDependencies(newVersion.definitions)
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
