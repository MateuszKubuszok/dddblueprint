package dddblueprint.compiler

import dddblueprint.{ input, output }
import monocle.Iso

trait RefIso {

  def snapshot: output.Snapshot

  private lazy val domainRefOutIn = snapshot.namespaces.domains.view.mapValues {
    case output.DomainName(name) => input.DomainRef(name)
  }.toMap
  private lazy val domainRefInOut = domainRefOutIn.map(_.swap).toMap

  implicit lazy val domainRefIso: Iso[input.DomainRef, output.DomainRef] =
    Iso[input.DomainRef, output.DomainRef](domainRefInOut)(domainRefOutIn)

  private lazy val definitionRefOutIn = snapshot.namespaces.definitions.view.mapValues {
    case output.DefinitionName(domainRef, name) => input.DefinitionRef(domainRefOutIn(domainRef), name)
  }.toMap
  private lazy val definitionRefInOut = definitionRefOutIn.map(_.swap).toMap

  implicit lazy val definitionRefIso: Iso[input.DefinitionRef, output.DefinitionRef] =
    Iso[input.DefinitionRef, output.DefinitionRef](definitionRefInOut)(definitionRefOutIn)
}
