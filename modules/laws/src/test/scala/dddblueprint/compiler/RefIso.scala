package dddblueprint
package compiler

import monocle.Iso

class RefIso(snapshot: output.Snapshot) {

  private val domainRefOutIn = snapshot.namespaces.domains.mapValues {
    case output.DomainName(name) => input.DomainRef(name)
  }
  private val domainRefInOut = domainRefOutIn.map(_.swap)

  implicit val domainRefIso: Iso[input.DomainRef, output.DomainRef] =
    Iso[input.DomainRef, output.DomainRef](domainRefInOut)(domainRefOutIn)

  private val definitionRefOutIn = snapshot.namespaces.definitions.mapValues {
    case output.DefinitionName(domainRef, name) => input.DefinitionRef(domainRefOutIn(domainRef), name)
  }
  private val definitionRefInOut = definitionRefOutIn.map(_.swap)

  implicit val definitionRefIso: Iso[input.DefinitionRef, output.DefinitionRef] =
    Iso[input.DefinitionRef, output.DefinitionRef](definitionRefInOut)(definitionRefOutIn)
}
