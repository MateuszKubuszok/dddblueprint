package dddblueprint
package validated

import scala.collection.immutable.{ ListMap, ListSet }

sealed trait Data extends ADT
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

    final case class Enum(definition: DefinitionRef, `type`: Enumerable, values: ListSet[String]) extends Domain

    sealed abstract class Record(val definition: DefinitionRef, val fields: Record.FieldSet) extends Domain
    object Record {
      type FieldSet = ListMap[String, Data]

      final case class FreeRecord(override val definition: DefinitionRef, override val fields: Record.FieldSet)
          extends Record(definition, fields)

      final case class Entity(override val definition: DefinitionRef, override val fields: Record.FieldSet)
          extends Record(definition, fields)
      final case class Value(override val definition: DefinitionRef, override val fields: Record.FieldSet)
          extends Record(definition, fields)
      final case class Event(override val definition: DefinitionRef, override val fields: Record.FieldSet)
          extends Record(definition, fields)
    }
  }
}
