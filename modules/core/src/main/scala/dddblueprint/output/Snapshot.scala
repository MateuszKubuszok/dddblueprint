package dddblueprint
package output

import cats.Eq
import cats.derived.ShowPretty
import cats.implicits._
import monocle.function.Index
import monocle.macros.syntax.lens._
import monocle.macros.GenLens
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

@Semi(Eq, ShowPretty) final case class Snapshot(
  namespaces:       Namespaces                                     = Namespaces(),
  domains:          ListMap[DomainRef, Definitions]                = ListMap.empty,
  version:          Int                                            = 0,
  manualMigrations: ListMap[DefinitionRef, ListSet[DefinitionRef]] = ListMap.empty
) {

  def bumpVersion: Snapshot = copy(version = version + 1, manualMigrations = ListMap.empty)

  lazy val definitions: ListMap[DefinitionRef, Data.Definition] =
    domains.values.map(_.definitions).foldLeft(ListMap.empty[DefinitionRef, Data.Definition])(_ ++ _)

  def findDomain(ref: output.DefinitionRef): Option[DomainRef] =
    domains.mapValues(_.definitions.keys.toSet).collectFirst {
      case (domainRef, definitionRefs) if definitionRefs.contains(ref) => domainRef
    }

  private def domainIndex(ref: output.DomainRef) =
    Index.fromAt[ListMap[output.DomainRef, output.Definitions], output.DomainRef, output.Definitions].index(ref)

  private val intoDefinitions =
    GenLens[output.Definitions](_.definitions)

  def withDefinition(domainRef: DomainRef, definitionRef: DefinitionRef, body: Data.Definition): Snapshot =
    this
    // ensure domain key exists
      .lens(_.domains)
      .modify(ListMap(domainRef -> Definitions()) ++ _)
      // ensure definition key exists
      .lens(_.domains)
      .composeOptional(domainIndex(domainRef))
      .composeLens(intoDefinitions)
      .modify(_.updated(definitionRef, body))

  def withoutDefinition(domainRef: DomainRef, definitionRef: DefinitionRef): Snapshot =
    this
    // ensure domain key exists
      .lens(_.domains)
      .modify(ListMap(domainRef -> Definitions()) ++ _)
      // remove definition key
      .lens(_.domains)
      .composeOptional(domainIndex(domainRef))
      .composeLens(intoDefinitions)
      .modify(_ - definitionRef)
}
