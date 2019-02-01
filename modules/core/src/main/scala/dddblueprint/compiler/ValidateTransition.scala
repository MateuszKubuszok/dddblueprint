package dddblueprint
package compiler

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl.implicits._
import io.scalaland.pulp.Cached

import scala.collection.immutable.ListSet

@Cached class ValidateTransition[F[_]: Monad: SchemaErrorRaise] {

  // TODO: removed definitions - ensure not used?
  // TODO: removed enum values - ensure not used? - add required migrations defs
  // TODO: removed records fields - ensure not used? - add required migrations defs
  // TODO: definition update - has type not changed
  def apply(oldVersion: output.Snapshot, newVersion: output.Snapshot): F[Unit] = {
    oldVersion.hashCode()
    newVersion.hashCode()
    ().pure[F]
  }

  val argumentToRef: output.Argument => Option[output.DefinitionRef] = {
    case ref: output.DefinitionRef  => Some(ref)
    case _:   output.Data.Primitive => None
    case output.Data.Definition.Record.Tuple(ref, _) => Some(ref)
  }

  // TODO: potentially throwing, fix later
  // use new
  // fallback on old
  // add some mock if not found
  def refToDomainAndName(version: output.Snapshot, ref: output.DefinitionRef): (String, String) = {
    val output.DefinitionName(domainRef, name) = version.namespaces.definitions(ref)
    val output.DomainName(domainName)          = version.namespaces.domains(domainRef)
    domainName -> name
  }

  def removedAreNotUsed(newVersion: output.Snapshot): F[Unit] = {
    val allDefinitions = newVersion.domains.to[ListSet].flatMap(_._2.definitions.to[ListSet])
    val isDefined      = allDefinitions.map(_._1).contains _
    allDefinitions.map(_._2).toList.flatMap {
      case _: output.Data.Definition.Enum => List.empty[SchemaError]

      case output.Data.Definition.Record.Aux(ref, fields) =>
        val (domain, name) = refToDomainAndName(newVersion, ref)
        val usedRefs       = fields.mapValues(argumentToRef).collect { case (f, Some(r)) => f -> r }
        val refNames       = usedRefs.map(_.swap)
        usedRefs.values.filterNot(isDefined).toList.map { refWithoutDef =>
          SchemaError.FieldDefinitionMissing(domain = domain, name = name, field = refNames(refWithoutDef)): SchemaError
        }

      case _ => List.empty[SchemaError] // TODO: services, etc
    } match {
      case head :: tail => NonEmptyList(head, tail).raise[F, Unit]
      case _            => ().pure[F]
    }
  }
}
