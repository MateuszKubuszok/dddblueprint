package dddblueprint
package output

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.ShowPretty
import cats.implicits._
import io.scalaland.catnip.Semi
import monocle.function.Index
import monocle.macros.syntax.lens._
import monocle.macros.GenLens

@Semi(Eq, ShowPretty) final case class Snapshot(
  namespaces:          Namespaces                                           = Namespaces(),
  domains:             ListMap[DomainRef, Definitions]                      = ListMap.empty,
  version:             Int                                                  = 0,
  automaticMigrations: ListMap[DefinitionRef, Dependencies]                 = ListMap.empty,
  manualMigrations:    ListMap[DefinitionRef, Dependencies]                 = ListMap.empty,
  applicableDiffs:     ListMap[DefinitionRef, NonEmptyList[ApplicableDiff]] = ListMap.empty,
  manualDiffs:         ListMap[DefinitionRef, NonEmptyList[ManualDiff]]     = ListMap.empty
) {

  def bumpVersion: Snapshot =
    copy(
      version             = version + 1,
      automaticMigrations = ListMap.empty,
      manualMigrations    = ListMap.empty,
      applicableDiffs     = ListMap.empty,
      manualDiffs         = ListMap.empty
    )

  lazy val definitions: ListMap[DefinitionRef, Data.Definition] =
    domains.values.map(_.definitions).foldLeft(ListMap.empty[DefinitionRef, Data.Definition])(_ ++ _)

  def findDomain(ref: output.DefinitionRef): Option[DomainRef] =
    domains.mapValues(_.definitions.keys.toSet).collectFirst {
      case (domainRef, definitionRefs) if definitionRefs.contains(ref) => domainRef
    }

  def findName(ref: output.DefinitionRef): Option[String] =
    for {
      domainRef <- findDomain(ref)
      domainName <- namespaces.domains.get(domainRef)
      name <- namespaces.definitions.get(ref)
    } yield s"$domainName.$name"

  def findDomainNameAndName(ref: DefinitionRef): Option[(String, String)] =
    for {
      domainRef <- findDomain(ref)
      domainName <- namespaces.domains.get(domainRef)
      name <- findName(ref)
    } yield domainName.name -> name

  private def domainIndex(ref: output.DomainRef) =
    Index.fromAt[ListMap[output.DomainRef, output.Definitions], output.DomainRef, output.Definitions].index(ref)

  private val intoDefinitions =
    GenLens[output.Definitions](_.definitions)

  // ensures domain exists in namespace
  def withDomainRef(domainRef: DomainRef, name: String): Snapshot =
    this.lens(_.namespaces.domains).modify(_ + (domainRef -> output.DomainName(name)))

  // ensures definitions exists in namespace
  def withDefinitionRef(domainRef:     DomainRef,
                        domainName:    String,
                        definitionRef: DefinitionRef,
                        name:          String): Snapshot =
    this
      .withDomainRef(domainRef, domainName)
      .lens(_.namespaces.definitions)
      .modify(
        _ + (definitionRef -> output.DefinitionName(domainRef, name))
      )

  // assumes refs is already put into namespaces
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

  // ensures refs and definitions are in place
  def withDefinition(domainRef:     DomainRef,
                     domainName:    String,
                     definitionRef: DefinitionRef,
                     name:          String,
                     body:          Data.Definition): Snapshot =
    this.withDefinitionRef(domainRef, domainName, definitionRef, name).withDefinition(domainRef, definitionRef, body)

  // removes definition if there is something to remove
  def withoutDefinition(domainRef: DomainRef, defRef: DefinitionRef): Snapshot =
    this
    // ensure domain key exists
      .lens(_.domains)
      .modify(ListMap(domainRef -> Definitions()) ++ _)
      // remove definition key
      .lens(_.domains)
      .composeOptional(domainIndex(domainRef))
      .composeLens(intoDefinitions)
      .modify(_ - defRef)
      // remove all usages
      .lens(_.domains)
      .composeOptional(domainIndex(domainRef))
      .composeLens(intoDefinitions)
      .modify {
        _.map {
          case (ref, enum: output.Data.Definition.Enum) =>
            ref -> enum
          case (ref, record: output.Data.Definition.Record) =>
            ref -> record.withFields(record.fields.filterNot(_._2 === defRef))
          case (ref, service: output.Data.Definition.Service) =>
            ref -> service.lens(_.input).modify(_.filterNot(_._2 === defRef)).lens(_.output).modify(_ - defRef)
          case (ref, publisher: output.Data.Definition.Publisher) =>
            ref -> publisher.lens(_.events).modify(_ - defRef)
          case (ref, subscriber: output.Data.Definition.Subscriber) =>
            ref -> subscriber.lens(_.events).modify(_ - defRef)
        } - defRef
      }
      // remove from namespaces
      .lens(_.namespaces.definitions)
      .modify(_ - defRef)

  // assumes definition exists
  def renameDefinition(domainRef: DomainRef, defRef: DefinitionRef, newName: String): Snapshot =
    this.lens(_.namespaces.definitions).modify(_.updated(defRef, output.DefinitionName(domainRef, newName)))
}
