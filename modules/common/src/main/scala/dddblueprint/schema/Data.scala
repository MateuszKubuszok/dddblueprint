package dddblueprint
package schema


import scala.collection.immutable.{ ListMap, ListSet }

sealed abstract class Data extends ADT
object Data {

  sealed trait Primitive extends Data
  sealed trait Enumerable extends Primitive
  object Primitive {

    case object ID extends Primitive
    case object Boolean extends Enumerable
    case object Int extends Enumerable
    case object Long extends Enumerable
    case object Float extends Enumerable
    case object Double extends Enumerable
    case object String extends Enumerable
  }

  sealed trait Domain extends Data
  object Domain {

    final case class Enum(domain:   DomainRef, name: String, values: ListSet[String], `type`: Enumerable) extends Domain

    type FieldSet = ListMap[String, Data]

    sealed trait Record extends Domain
    object Record {

      final case class FreeRecord(fields: FieldSet) extends Record

      final case class Entity(domain: DomainRef, name: String, fields: FieldSet) extends Record
      final case class Value(domain:  DomainRef, name: String, fields: FieldSet) extends Record
      final case class Event(domain:  DomainRef, name: String, fields: FieldSet) extends Record
    }
  }
}
