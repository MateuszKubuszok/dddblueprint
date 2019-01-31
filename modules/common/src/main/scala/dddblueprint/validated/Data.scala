package dddblueprint
package validated

import cats.{ Eq, Show }, cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

@Semi(Eq, Show) sealed trait Data extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object Data {

  @Semi(Eq, Show) sealed trait Primitive extends Data
  @Semi(Eq, Show) sealed trait Enumerable extends Data

  case object ID extends Primitive
  case object Boolean extends Primitive with Enumerable
  case object Int extends Primitive with Enumerable
  case object Long extends Primitive with Enumerable
  case object Float extends Primitive with Enumerable
  case object Double extends Primitive with Enumerable
  case object String extends Primitive with Enumerable

  @Semi(Eq, Show) sealed abstract class Definition(val ref: DefinitionRef) extends Data
  object Definition {

    @Semi(Eq, Show) final case class Enum(override val ref: DefinitionRef, values: ListSet[String], `type`: Enumerable)
        extends Definition(ref)

    type FieldSet = ListMap[String, Data]

    @Semi(Eq, Show) sealed abstract class Record(override val ref: DefinitionRef, val fields: FieldSet)
        extends Definition(ref)
    object Record {
      type FieldSet = ListMap[String, Data]

      @Semi(Eq, Show) final case class Tuple(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields)

      @Semi(Eq, Show) final case class Entity(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields)
      @Semi(Eq, Show) final case class Value(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields)
      @Semi(Eq, Show) final case class Event(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields)
    }

    @Semi(Eq, Show) final case class Service(override val ref: DefinitionRef,
                                             input:            ListSet[DefinitionRef],
                                             output:           ListSet[DefinitionRef])
        extends Definition(ref)
    @Semi(Eq, Show) final case class Publisher(override val ref: DefinitionRef, events: ListSet[DefinitionRef])
        extends Definition(ref)
    @Semi(Eq, Show) final case class Subscriber(override val ref: DefinitionRef, events: ListSet[DefinitionRef])
        extends Definition(ref)
  }
}
