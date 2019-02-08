package dddblueprint
package compiler

import monocle.Iso

trait RefIso {

  def snapshot: output.Snapshot

  private lazy val domainRefOutIn = snapshot.namespaces.domains.mapValues {
    case output.DomainName(name) => input.DomainRef(name)
  }
  private lazy val domainRefInOut = domainRefOutIn.map(_.swap)

  implicit lazy val domainRefIso: Iso[input.DomainRef, output.DomainRef] =
    Iso[input.DomainRef, output.DomainRef](domainRefInOut)(domainRefOutIn)

  private lazy val definitionRefOutIn = snapshot.namespaces.definitions.mapValues {
    case output.DefinitionName(domainRef, name) => input.DefinitionRef(domainRefOutIn(domainRef), name)
  }
  private lazy val definitionRefInOut = definitionRefOutIn.map(_.swap)

  implicit lazy val definitionRefIso: Iso[input.DefinitionRef, output.DefinitionRef] =
    Iso[input.DefinitionRef, output.DefinitionRef](definitionRefInOut)(definitionRefOutIn)
}
