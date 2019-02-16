package dddblueprint
package compiler

import java.util.UUID

import cats.{ Eq, Semigroup, Traverse }
import cats.data.NonEmptyList
import cats.derived.ShowPretty
import cats.effect.Sync
import cats.implicits._
import cats.mtl.implicits._
import dddblueprint.output.Dependencies
import io.scalaland.catnip.Semi
import io.scalaland.pulp.Cached
import monocle.macros.syntax.lens._

import scala.collection.immutable.{ ListMap, ListSet }

@Cached class ValidateTransition[F[_]: Sync: SnapshotState: SchemaErrorRaise] {

  private val mockDefinition = output.DefinitionName(output.DomainRef(new UUID(0, 0)), "undefined")
  private val mockDomainName = output.DomainName("undefined")
  private def refToDomainAndName(version: output.Snapshot, ref: output.DefinitionRef): (String, String) = {
    val output.DefinitionName(domainRef, name) = version.namespaces.definitions.getOrElse(ref, mockDefinition)
    val output.DomainName(domainName)          = version.namespaces.domains.getOrElse(domainRef, mockDomainName)
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
          .map2(calculateMigrations(oldVersion, newVersion, dependencies))(_ |+| _)
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
                                dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] =
    Sync[F].defer {
      dependencies.toList.flatMap {
        case (ref, output.Dependencies(direct, _)) =>
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
          } yield
            SchemaError.DefinitionTypeMismatch(domain, name, "event", newVersion.definitions(depsRef)): SchemaError
      } match {
        case head :: tail => NonEmptyList[SchemaError](head, tail).raise[F, Unit]
        case Nil          => ().pure[F]
      }
    }

  def eventPublishedInTheirDomain(newVersion:   output.Snapshot,
                                  dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] =
    Sync[F].defer {
      dependencies.toList.flatMap {
        case (ref, output.Dependencies(direct, _)) =>
          for {
            publisher <- newVersion.definitions.get(ref).toList.collect {
              case p: output.Data.Definition.Publisher => p
            }
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

  // scalastyle:off method.length
  /* Logic behind calculating dependencies:
   *
   *
   * for enums:
   * - if relationship was added, default value is missing - user should handle it manually
   * - if value was added, old values maps to new ones without issues - we can generate migration
   * - if relationship was removed, we have extra value - we can generate migration
   * - if value was removed, we have unhandled value - user should handle it manually
   * - if relationship's type changed, it requires extra logic - user should handle it manually
   * - if values were added and removed, it requires extra logic - user should handle it manually
   * - if nothing changed, we are on the same version
   *
   * +--------------+--------------+----------------+----------------+---------------+
   * |              | unchanged    | added          | removed        | added/removed | C.c value
   * +--------------+--------------+----------------+----------------+---------------+
   * | unchanged    | same version | old values map | unmapped value | custom logic  |
   * |              |              | automatic      | manual         | manual        |
   * +--------------+--------------+----------------+----------------+---------------+
   * | added        | default missing                                                |
   * |              | manual                                                         |
   * +--------------+--------------+----------------+----------------+---------------+
   * | removed      | extra value                                                    |
   * |              | automatic                                                      |
   * +--------------+--------------+----------------+----------------+---------------+
   * | changed type | custom logic                                                   |
   * |              | manual                                                         |
   * +--------------+--------------+----------------+----------------+---------------+
   *   A.b -> B enum
   *   dependency
   *
   *
   * for records:
   *
   * - if relationship/field was added, default value is missing - user should handle it manually
   * - if relationship/field was removed, we have extra value - we can generate migration
   * - if relationship's/field's type changed, it requires extra logic - user should handle it manually
   * - if nothing changed, we are on the same version
   *
   * +--------------+--------------+-----------------+-------------+--------------+
   * |              | unchanged    | added           | removed     | changed type | B.c field
   * +--------------+--------------+-----------------+-------------+--------------+
   * | unchanged    | same version | default missing | extra field | custom logic |
   * |              |              | manual          | automatic   | manual       |
   * +--------------+--------------+-----------------+-------------+--------------+
   * | added        | default missing                                             |
   * |              | manual                                                      |
   * +--------------+--------------+-----------------+-------------+--------------+
   * | removed      | extra field                                                 |
   * |              | automatic                                                   |
   * +--------------+--------------+-----------------+-------------+--------------+
   * | changed type | custom logic                                                |
   * |              | manual                                                      |
   * +--------------+--------------+-----------------+-------------+--------------+
   *   A.b -> B record
   *   dependency
   *
   *
   * Summary:
   *   if relationship added or relationship's type changes -> manual
   *   else if relationship removed -> automatic
   *   else
   *     if dependency is enum and some value was removed -> manual
   *     else if dependency is values were only added -> automatic
   *     else if dependency is record and some field was added/changed -> manual
   *     else if dependency is record and fields were only removed -> automatic
   *     else unchanged
   *
   * Definition can have both automatic and manual migrations its dependencies have different circumstances.
   */
  def calculateMigrations(oldVersion:   output.Snapshot,
                          newVersion:   output.Snapshot,
                          dependencies: ListMap[output.DefinitionRef, Dependencies]): F[Unit] = Sync[F].defer {
    val enumsWithRemovedValues = for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      if oldRef === newRef && !oldDef.values.subsetOf(newDef.values)
    } yield newRef

    val enumsWithOnlyAddedValues = (for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, enum: output.Data.Definition.Enum) => ref -> enum
      }
      if oldRef === newRef && !newDef.values.subsetOf(oldDef.values)
    } yield newRef) -- enumsWithRemovedValues

    val recordsWithAddedOrChangedFields = for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, record: output.Data.Definition.Record) => ref -> record
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, record: output.Data.Definition.Record) => ref -> record
      }
      if oldRef === newRef
      fieldAdded = !newDef.fields.keySet.subsetOf(oldDef.fields.keySet)
      fieldChanged = (for {
        (name1, arg1) <- oldDef.fields
        (name2, arg2) <- newDef.fields
        if name1 === name2
      } yield arg1.argumentType =!= arg2.argumentType).exists(identity)
      if fieldAdded || fieldChanged
    } yield newRef

    val recordsWithOnlyRemovedFields = (for {
      (newRef, newDef) <- newVersion.definitions.to[ListSet].collect {
        case (ref, record: output.Data.Definition.Record) => ref -> record
      }
      (oldRef, oldDef) <- oldVersion.definitions.to[ListSet].collect {
        case (ref, record: output.Data.Definition.Record) => ref -> record
      }
      if oldRef === newRef
      fieldRemoved = !oldDef.fields.keySet.subsetOf(newDef.fields.keySet)
      if fieldRemoved
    } yield newRef) -- recordsWithAddedOrChangedFields

    val automaticOnItsOwn = recordsWithOnlyRemovedFields ++ enumsWithOnlyAddedValues
    val immediatelyManual = recordsWithAddedOrChangedFields ++ enumsWithRemovedValues

    import ValidateTransition.MigrationType

    def findType(ref: output.DefinitionRef, solution: Map[output.DefinitionRef, MigrationType]): Option[MigrationType] =
      solution.get(ref) match {
        case Some(mt) => Some(mt)
        case None =>
          val output.Dependencies(direct, transitive) = dependencies.getOrElse(ref, output.Dependencies(ListSet.empty))
          val affected                                = transitive + ref

          if (immediatelyManual.intersect(affected).nonEmpty) Some(MigrationType.Manual)
          else if (affected.subsetOf(automaticOnItsOwn)) Some(MigrationType.Automatic)
          else if (immediatelyManual.intersect(affected).isEmpty && automaticOnItsOwn.intersect(affected).isEmpty) {
            Some(MigrationType.Unchanged)
          } else {
            direct
              .map(solution.get)
              .reduceOption { (aOpt, bOpt) =>
                for {
                  a <- aOpt
                  b <- bOpt
                } yield a |+| b
              }
              .flatten
          }
      }

    val allRefs = newVersion.definitions.keySet

    @scala.annotation.tailrec
    def findAllMigrationTypes(
      solution: Map[output.DefinitionRef, MigrationType] = Map.empty
    ): Map[output.DefinitionRef, MigrationType] =
      if (solution.keySet === allRefs) solution
      else {
        findAllMigrationTypes(
          allRefs
            .map { ref =>
              ref -> findType(ref, solution)
            }
            .collect { case (k, Some(v)) => k -> v }
            .toMap
        )
      }

    val allMigrationTypes = findAllMigrationTypes()
    val allAutomatic      = allMigrationTypes.collect { case (ref, MigrationType.Automatic) => ref }.toSet
    val allManual         = allMigrationTypes.collect { case (ref, MigrationType.Manual) => ref }.toSet

    val migrations = allRefs.map { ref =>
      // this way we also handle definitions that have no dependencies
      val output.Dependencies(direct, transitive) = dependencies.getOrElse(ref, output.Dependencies(ListSet.empty))

      val affected            = transitive + ref
      val transitiveManual    = affected.intersect(allManual)
      val transitiveAutomatic = affected.intersect(allAutomatic)

      val automaticDeps =
        if (transitiveAutomatic.isEmpty) None
        else Some(output.Dependencies((direct + ref).intersect(transitiveAutomatic), transitiveAutomatic))
      val manualDeps =
        if (transitiveManual.isEmpty) None
        else Some(output.Dependencies((direct + ref).intersect(transitiveManual), transitiveManual))

      ref -> (automaticDeps -> manualDeps)
    }

    Traverse[ListSet]
      .sequence(
        migrations
          .map {
            case (ref, (automaticOpt, manualOpt)) =>
              type Deps = ListMap[output.DefinitionRef, output.Dependencies]
              val automaticUpdate: Deps => Deps = automaticOpt match {
                case Some(automatic) => _.updated(ref, automatic)
                case None            => identity
              }
              val manualUpdate: Deps => Deps = manualOpt match {
                case Some(manual) => _.updated(ref, manual)
                case None         => identity
              }
              SnapshotState[F].modify(
                _.lens(_.automaticMigrations).modify(automaticUpdate).lens(_.manualMigrations).modify(manualUpdate)
              )
          }
          .to[ListSet]
      )
      .map(_ => ())
  }
  // scalastyle:on method.length

  /* Logic behind calculating versions:
 *
 * - if version was missing (definition didn't exist before) set it to 1
 * - if definition changed, is on manual or automatic migration list, increment it (once!)
 * - otherwise leave version intact
 */
}

object ValidateTransition {

  @Semi(Eq, ShowPretty) sealed trait MigrationType extends ADT
  @SuppressWarnings(Array("org.wartremover.warts.Equals")) object MigrationType {
    case object Automatic extends MigrationType
    case object Manual extends MigrationType
    case object Unchanged extends MigrationType

    implicit val semigroup: Semigroup[MigrationType] = (x: MigrationType, y: MigrationType) =>
      (x, y) match {
        case (Manual, _)    => Manual
        case (_, Manual)    => Manual
        case (Automatic, _) => Automatic
        case (_, Automatic) => Automatic
        case _              => Unchanged
    }
  }

  @inline def apply[F[_]](implicit validateTransition: ValidateTransition[F]): ValidateTransition[F] =
    validateTransition
}
