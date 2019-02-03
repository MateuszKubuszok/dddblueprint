package dddblueprint
package compiler

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl.implicits._
import io.scalaland.pulp.Cached

import scala.collection.immutable.{ ListMap, ListSet }

@Cached class ValidateTransition[F[_]: Monad: SchemaErrorRaise] {

  private val argumentToRef: output.Argument => Option[output.DefinitionRef] = {
    case ref: output.DefinitionRef  => Some(ref)
    case _:   output.Data.Primitive => None
    case output.Data.Definition.Record.Tuple(ref, _) => Some(ref)
  }

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
    for {
      _ <- removedAreNotUsed(newVersion)
      _ <- typesMatches(oldVersion, newVersion)
      _ <- migrationsForEnums(oldVersion, newVersion)
      _ <- migrationsForRecords(oldVersion, newVersion)
    } yield ()

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

  // scalastyle:off cyclomatic.complexity
  def removedAreNotUsed(newVersion: output.Snapshot): F[Unit] = {
    val allDefinitions = newVersion.domains.to[ListSet].flatMap(_._2.definitions.to[ListSet])
    val isDefined      = allDefinitions.map(_._1).contains _
    allDefinitions.map(_._2).toList.flatMap {
      case output.Data.Definition.Record.Aux(ref, fields) =>
        findMissing(newVersion, ref, isDefined) {
          fields.map { case (n, a) => n -> argumentToRef(a) }.collect { case (f, Some(r)) => f -> r }
        }

      case output.Data.Definition.Service(ref, inputs, outputs) =>
        findMissing(newVersion, ref, isDefined) {
          // TODO: output # -> entity name
          inputs.map { case (n, a) => n -> argumentToRef(a) }.collect { case (f, Some(r)) => f -> r } ++ ListMap(
            outputs.zipWithIndex.map { case (r, i) => s"output $i" -> r }.toSeq: _*
          )
        }

      case output.Data.Definition.Publisher(ref, events) =>
        findMissing(newVersion, ref, isDefined) {
          // TODO: event # -> entity name
          ListMap(events.zipWithIndex.map { case (r, i) => s"event $i" -> r }.toSeq: _*)
        }

      case output.Data.Definition.Subscriber(ref, events) =>
        findMissing(newVersion, ref, isDefined) {
          // TODO: event # -> entity name
          ListMap(events.zipWithIndex.map { case (r, i) => s"event $i" -> r }.toSeq: _*)
        }

      case _ => List.empty[SchemaError]
    } match {
      case head :: tail => NonEmptyList(head, tail).raise[F, Unit]
      case _            => ().pure[F]
    }
  }
  // scalastyle:on
  
  def typesMatches(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] =
    (for {
      (newRef, newDef) <- newVersion.domains.values.to[ListSet].flatMap(_.definitions.to[ListSet])
      (oldRef, oldDef) <- oldVersion.domains.values.to[ListSet].flatMap(_.definitions.to[ListSet])
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
        case (_: output.Data.Definition.Record, other) =>
          ListSet(SchemaError.DefinitionTypeMismatch(domain, name, "record", other): SchemaError)

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
      case _            => ().pure[F]
    }

  // TODO: removed enum/enum values - add required migrations defs
  def migrationsForEnums(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = {
    oldVersion.hashCode()
    newVersion.hashCode()
    ().pure[F]
  }

  // TODO: removed records/fields - add required migrations defs
  def migrationsForRecords(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = {
    oldVersion.hashCode()
    newVersion.hashCode()
    ().pure[F]
  }
}
