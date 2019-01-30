package dddblueprint
package validated

import scala.collection.immutable.ListMap

final case class DomainSnapshot(
  definitions: ListMap[DefinitionRef, Data.Domain] = ListMap.empty
)

final case class Snapshot(
  domains: ListMap[DomainRef, DomainSnapshot] = ListMap.empty
)
