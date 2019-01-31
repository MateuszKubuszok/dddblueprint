package dddblueprint
package input

import cats.Eq, cats.derived.ShowPretty, cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

@Semi(Eq, ShowPretty) sealed trait Data extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object Data {

  @Semi(Eq, ShowPretty) sealed trait Primitive extends Data
  @Semi(Eq, ShowPretty) sealed trait Enumerable extends Data

  case object ID extends Primitive
  case object Boolean extends Primitive with Enumerable
  case object Int extends Primitive with Enumerable
  case object Long extends Primitive with Enumerable
  case object Float extends Primitive with Enumerable
  case object Double extends Primitive with Enumerable
  case object String extends Primitive with Enumerable

  @Semi(Eq, ShowPretty) sealed abstract class Definition(val ref: DefinitionRef) extends Data
  object Definition {

    @Semi(Eq, ShowPretty) final case class Enum(override val ref: DefinitionRef,
                                                values:           ListSet[String],
                                                `type`:           Enumerable)
        extends Definition(ref)

    type FieldSet = ListMap[String, Data]

    @Semi(Eq, ShowPretty) sealed abstract class Record(override val ref: DefinitionRef) extends Definition(ref)
    object Record {

      @Semi(Eq, ShowPretty) final case class Tuple(override val ref: DefinitionRef, fields: FieldSet)
          extends Record(ref)

      @Semi(Eq, ShowPretty) final case class Entity(override val ref: DefinitionRef, fields: FieldSet)
          extends Record(ref)
      @Semi(Eq, ShowPretty) final case class Value(override val ref: DefinitionRef, fields: FieldSet)
          extends Record(ref)
      @Semi(Eq, ShowPretty) final case class Event(override val ref: DefinitionRef, fields: FieldSet)
          extends Record(ref)
    }

    @Semi(Eq, ShowPretty) final case class Service(override val ref: DefinitionRef,
                                                   input:            ListSet[DefinitionRef],
                                                   output:           ListSet[DefinitionRef])
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Publisher(override val ref: DefinitionRef, events: ListSet[DefinitionRef])
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Subscriber(override val ref: DefinitionRef, events: ListSet[DefinitionRef])
        extends Definition(ref)
  }
}