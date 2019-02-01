package dddblueprint
package output

import java.util.UUID

import cats.{ Eq, Show }
import cats.derived.ShowPretty
import cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

@Semi(Eq, ShowPretty) sealed trait Argument extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals")) object Argument

@Semi(Eq, ShowPretty) final case class DefinitionRef(id: UUID = UUID.randomUUID) extends Argument

@Semi(Eq, ShowPretty) sealed trait Data extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object Data {

  @Semi(Eq, ShowPretty) sealed trait Primitive extends Data with Argument
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
        extends Definition(ref) {

      def withValues(newValues: ListSet[String]): Enum = copy(values = newValues)
    }

    type FieldSet = ListMap[String, Argument]
    type RefSet   = ListSet[DefinitionRef]

    @Semi(Eq, Show) sealed abstract class Record(override val ref: DefinitionRef, val fields: FieldSet)
        extends Definition(ref) {

      def withFields(newFields: FieldSet): Record
    }
    object Record {

      // putting unapply directly into Record breaks type class derivation
      object Aux {
        def unapply(record: Record): Option[(DefinitionRef, FieldSet)] = Some((record.ref, record.fields))
      }

      @Semi(Eq, ShowPretty) final case class Tuple(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields)
          with Argument {

        def withFields(newFields: FieldSet): Tuple = copy(fields = newFields)
      }

      @Semi(Eq, ShowPretty) final case class Entity(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields) {

        def withFields(newFields: FieldSet): Entity = copy(fields = newFields)
      }
      @Semi(Eq, ShowPretty) final case class Value(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields) {

        def withFields(newFields: FieldSet): Value = copy(fields = newFields)
      }
      @Semi(Eq, ShowPretty) final case class Event(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields) {

        def withFields(newFields: FieldSet): Event = copy(fields = newFields)
      }
    }

    @Semi(Eq, ShowPretty) final case class Service(override val ref: DefinitionRef, input: RefSet, output: RefSet)
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Publisher(override val ref: DefinitionRef, events: RefSet)
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Subscriber(override val ref: DefinitionRef, events: RefSet)
        extends Definition(ref)
  }
}
