package dddblueprint
package output

import java.util.UUID

import cats.{ Eq, Show }
import cats.derived.ShowPretty
import cats.implicits._
import io.scalaland.catnip.Semi

import scala.collection.immutable.{ ListMap, ListSet }

sealed trait Argument extends ADT {

  def argumentType: Argument.Type
}
object Argument {

  @Semi(Eq, ShowPretty) sealed trait Type extends ADT
  @SuppressWarnings(Array("org.wartremover.warts.Equals")) object Type {
    case object DefinitionRef extends Type
    case object Primitive extends Type
    case object Collection extends Type
    case object Tuple extends Type
  }

  // we aren't relying on derivation here as it leads to scala.UninitializedFieldError because of recursion

  implicit val eq: Eq[Argument] = (a: Argument, b: Argument) =>
    (a, b) match {
      case (x: DefinitionRef, y:   DefinitionRef)   => x === y
      case (x: Data.Primitive, y:  Data.Primitive)  => x === y
      case (x: Data.Collection, y: Data.Collection) => x === y
      case (x: Data.Tuple, y:      Data.Tuple)      => x === y
      case _ => false
  }
  implicit val show: ShowPretty[Argument] = {
    case x: DefinitionRef   => implicitly[ShowPretty[DefinitionRef]].showLines(x)
    case x: Data.Primitive  => implicitly[ShowPretty[Data.Primitive]].showLines(x)
    case x: Data.Collection => implicitly[ShowPretty[Data.Collection]].showLines(x)
    case x: Data.Tuple      => implicitly[ShowPretty[Data.Tuple]].showLines(x)
  }
}

@Semi(Eq, ShowPretty) final case class DefinitionRef(id: UUID = UUID.randomUUID) extends Argument {

  def argumentType: Argument.Type = Argument.Type.DefinitionRef
}

@Semi(Eq, ShowPretty) sealed trait Data extends ADT
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object Data {

  @Semi(Eq, ShowPretty) sealed trait Primitive extends Data with Argument {

    def argumentType: Argument.Type = Argument.Type.Primitive
  }
  @Semi(Eq, ShowPretty) sealed trait Enumerable

  case object UUID extends Primitive
  case object Boolean extends Primitive with Enumerable
  case object Int extends Primitive with Enumerable
  case object Long extends Primitive with Enumerable
  case object Float extends Primitive with Enumerable
  case object Double extends Primitive with Enumerable
  case object String extends Primitive with Enumerable

  object Primitive
  object Enumerable

  @Semi(Eq, ShowPretty) sealed trait Collection extends Data with Argument {

    def argumentType: Argument.Type = Argument.Type.Collection
  }
  object Collection {
    @Semi(Eq, ShowPretty) final case class Option(of: Argument) extends Collection
    @Semi(Eq, ShowPretty) final case class Array(of:  Argument) extends Collection
    @Semi(Eq, ShowPretty) final case class Set(of:    Argument) extends Collection
    @Semi(Eq, ShowPretty) final case class Map(key:   Argument, value: Argument) extends Collection
  }

  @Semi(Eq, ShowPretty) final case class Tuple(arguments: List[Argument]) extends Argument {

    def argumentType: Argument.Type = Argument.Type.Tuple
  }

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

    @Semi(Eq, Show) sealed abstract class Record(override val ref: DefinitionRef,
                                                 val fields:       FieldSet,
                                                 val `type`:       Record.Type)
        extends Definition(ref) {

      def withFields(newFields: FieldSet): Record
    }
    object Record {

      @Semi(Eq, Show) sealed trait Type extends ADT
      object Type {
        case object Entity extends Type
        case object Value extends Type
        case object Event extends Type
      }

      // putting unapply directly into Record breaks type class derivation
      object Aux {

        def apply(ref: DefinitionRef, fields: FieldSet, `type`: Type): Record = `type` match {
          case Type.Entity => Entity(ref, fields)
          case Type.Value  => Value(ref, fields)
          case Type.Event  => Event(ref, fields)
        }

        def unapply(record: Record): Option[(DefinitionRef, FieldSet, Type)] =
          Some((record.ref, record.fields, record.`type`))
      }

      @Semi(Eq, ShowPretty) final case class Entity(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields, Type.Entity) {

        def withFields(newFields: FieldSet): Entity = copy(fields = newFields)
      }
      @Semi(Eq, ShowPretty) final case class Value(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields, Type.Value) {

        def withFields(newFields: FieldSet): Value = copy(fields = newFields)
      }
      @Semi(Eq, ShowPretty) final case class Event(override val ref: DefinitionRef, override val fields: FieldSet)
          extends Record(ref, fields, Type.Event) {

        def withFields(newFields: FieldSet): Event = copy(fields = newFields)
      }
    }

    @Semi(Eq, ShowPretty) final case class Service(override val ref: DefinitionRef, input: FieldSet, output: RefSet)
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Publisher(override val ref: DefinitionRef, events: RefSet)
        extends Definition(ref)
    @Semi(Eq, ShowPretty) final case class Subscriber(override val ref: DefinitionRef, events: RefSet)
        extends Definition(ref)
  }
}
