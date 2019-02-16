package dddblueprint
package parser

import cats.implicits._
import cats.effect.Sync
import fastparse._
import input._
import ScalaWhitespace._
import fastparse.Parsed.{ Failure, Success }
import io.scalaland.pulp.Cached

import scala.collection.immutable.{ ListMap, ListSet }

// scalastyle:off method.name
// scalastyle:off number.of.methods
@SuppressWarnings(
  Array("org.wartremover.warts.Equals",
        "org.wartremover.warts.Null",
        "org.wartremover.warts.Var",
        "org.wartremover.warts.While")
)
object Parser {

  object Keyword {
    def `{`[_: P]: P[Unit] = P("{")
    def `}`[_: P]: P[Unit] = P("}")

    def `(`[_: P]: P[Unit] = P("(")
    def `)`[_: P]: P[Unit] = P(")")

    def `[`[_: P]: P[Unit] = P("[")
    def `]`[_: P]: P[Unit] = P("]")

    def `.`[_: P]: P[Unit] = P(".")
    def `,`[_: P]: P[Unit] = P(",")
    def `:`[_: P]: P[Unit] = P(":")
    def `?`[_: P]: P[Unit] = P("?")
    def `'`[_: P]: P[Unit] = P("'")

    def `->`[_: P]: P[Unit] = P("->")

    def domain[_: P]: P[Unit] = P(IgnoreCase("domain"))

    def create[_: P]: P[Unit] = P(IgnoreCase("create"))
    def remove[_: P]: P[Unit] = P(IgnoreCase("remove"))
    def rename[_: P]: P[Unit] = P(IgnoreCase("rename"))

    def enum[_:       P]: P[Unit] = P(IgnoreCase("enum"))
    def entity[_:     P]: P[Unit] = P(IgnoreCase("entity"))
    def value[_:      P]: P[Unit] = P(IgnoreCase("value"))
    def event[_:      P]: P[Unit] = P(IgnoreCase("event"))
    def record[_:     P]: P[Unit] = P(IgnoreCase("record"))
    def service[_:    P]: P[Unit] = P(IgnoreCase("service"))
    def publisher[_:  P]: P[Unit] = P(IgnoreCase("publisher"))
    def subscriber[_: P]: P[Unit] = P(IgnoreCase("subscriber"))

    def fields[_: P]: P[Unit] = P(IgnoreCase("fields"))
    def values[_: P]: P[Unit] = P(IgnoreCase("values"))
    def of[_:     P]: P[Unit] = P(IgnoreCase("of"))

    def uuid[_:    P]: P[Data.UUID.type]    = P(IgnoreCase("uuid")).map(_ => Data.UUID)
    def boolean[_: P]: P[Data.Boolean.type] = P(IgnoreCase("boolean")).map(_ => Data.Boolean)
    def int[_:     P]: P[Data.Int.type]     = P(IgnoreCase("int")).map(_ => Data.Int)
    def long[_:    P]: P[Data.Long.type]    = P(IgnoreCase("long")).map(_ => Data.Long)
    def float[_:   P]: P[Data.Float.type]   = P(IgnoreCase("float")).map(_ => Data.Float)
    def double[_:  P]: P[Data.Double.type]  = P(IgnoreCase("double")).map(_ => Data.Double)
    def string[_:  P]: P[Data.String.type]  = P(IgnoreCase("string")).map(_ => Data.String)
  }

  object Input {
    def name[_: P]: P[String] = P(
      ((CharIn("a-z") | CharIn("A-Z") | "_" | "-") ~ (CharIn("0-9") | CharIn("a-z") | CharIn("A-Z") | "_" | "-").rep).!
    )
    def number[_: P]: P[String] = P(CharIn("0-9").rep.!)
    def text[_:   P]: P[String] = P(Keyword.`'` ~ CharsWhile(_ =!= '\'').! ~ Keyword.`'`)
  }

  object NonTerminal {
    import Keyword._
    import Input._

    // data

    def enumerable[_: P]: P[Data.Enumerable] = P(boolean | int | long | float | double | string)
    def primitive[_:  P]: P[Data.Primitive]  = P(uuid | boolean | int | long | float | double | string)

    def definitionRef[_: P]: P[DomainRef => DefinitionRef] = P(
      (name ~ `.` ~ name).map {
        case (domain, name) =>
          (_: DomainRef) =>
            DefinitionRef(DomainRef(domain), name)
      } |
        name.map { name => (domainRef: DomainRef) =>
          DefinitionRef(domainRef, name)
        }
    )

    def option[_: P]: P[DomainRef => Data.Collection.Option] =
      P(`?` ~ argument ~ `?`).map(_.andThen(Data.Collection.Option.apply))
    def array[_: P]: P[DomainRef => Data.Collection.Array] =
      P(`[` ~ argument ~ `]`).map(_.andThen(Data.Collection.Array.apply))
    def set[_: P]: P[DomainRef => Data.Collection.Set] =
      P(`{` ~ argument ~ `}`).map(_.andThen(Data.Collection.Set.apply))
    def map[_: P]: P[DomainRef => Data.Collection.Map] =
      P(`{` ~ argument ~ `:` ~ argument ~ `}`).map {
        case (k, v) =>
          (domainRef: DomainRef) =>
            Data.Collection.Map(k(domainRef), v(domainRef))
      }
    def collection[_: P]: P[DomainRef => Data.Collection] = P(option | array | set | map)

    def tuple[_: P]: P[DomainRef => Data.Tuple] = P(`(` ~ argument.rep(1, `,`) ~ `)`).map {
      args => (domainRef: DomainRef) =>
        Data.Tuple(args.map(_(domainRef)).toList)
    }

    def argument[_: P]: P[DomainRef => Argument] =
      P(primitive.map(ref => (_: DomainRef) => ref) | collection | tuple | definitionRef)
    def fieldSet[_: P]: P[DomainRef => Data.Definition.FieldSet] =
      P(`{` ~/ (name ~ `:` ~ argument).rep(sep = `,`) ~ `}`).map { arguments => (domainRef: DomainRef) =>
        ListMap(arguments.map { case (k, fv) => k -> fv(domainRef) }.toSeq: _*)
      }
    def refSet[_: P]: P[DomainRef => Data.Definition.RefSet] =
      P(`(` ~/ definitionRef.rep(sep = `,`) ~ `)`).map { definitionRefs => (domainRef: DomainRef) =>
        definitionRefs.map(_(domainRef)).to[ListSet]
      }

    // definitions

    def enumDefinition[_: P]: P[DomainRef => Data.Definition.Enum] =
      P(enum ~ of ~ enumerable ~ name ~ `(` ~/ (text | number).rep(sep = `,`) ~ `)`).map {
        case (etype, name, values) =>
          (domainRef: DomainRef) =>
            Data.Definition.Enum(DefinitionRef(domainRef, name), values.to[ListSet], etype)
      }

    def entityDefinition[_: P]: P[DomainRef => Data.Definition.Record.Entity] =
      P(entity ~ name ~ fieldSet).map {
        case (name, fields) =>
          (domainRef: DomainRef) =>
            Data.Definition.Record.Entity(DefinitionRef(domainRef, name), fields(domainRef))
      }
    def valueDefinition[_: P]: P[DomainRef => Data.Definition.Record.Value] =
      P(value ~ name ~ fieldSet).map {
        case (name, fields) =>
          (domainRef: DomainRef) =>
            Data.Definition.Record.Value(DefinitionRef(domainRef, name), fields(domainRef))
      }
    def eventDefinition[_: P]: P[DomainRef => Data.Definition.Record.Event] =
      P(event ~ name ~ fieldSet).map {
        case (name, fields) =>
          (domainRef: DomainRef) =>
            Data.Definition.Record.Event(DefinitionRef(domainRef, name), fields(domainRef))
      }
    def recordDefinition[_: P]: P[DomainRef => Data.Definition.Record] =
      P(entityDefinition | valueDefinition | eventDefinition)

    def serviceDefinition[_: P]: P[DomainRef => Data.Definition.Service] =
      P(service ~ name ~ fieldSet ~ `->` ~ refSet).map {
        case (name, inputs, outputs) =>
          (domainRef: DomainRef) =>
            Data.Definition.Service(DefinitionRef(domainRef, name), inputs(domainRef), outputs(domainRef))
      }

    def publisherDefinition[_: P]: P[DomainRef => Data.Definition.Publisher] =
      P(publisher ~ name ~ refSet).map {
        case (name, events) =>
          (domainRef: DomainRef) =>
            Data.Definition.Publisher(DefinitionRef(domainRef, name), events(domainRef))
      }

    def subscriberDefinition[_: P]: P[DomainRef => Data.Definition.Subscriber] =
      P(subscriber ~ name ~ refSet).map {
        case (name, events) =>
          (domainRef: DomainRef) =>
            Data.Definition.Subscriber(DefinitionRef(domainRef, name), events(domainRef))
      }

    def definition[_: P]: P[DomainRef => Data.Definition] =
      P(enumDefinition | recordDefinition | serviceDefinition | publisherDefinition | subscriberDefinition)

    // actions

    def createDefinitionAction[_: P]: P[DomainRef => Action.CreateDefinition] =
      P(create ~/ definition).map { definitionf => (domainRef: DomainRef) =>
        Action.CreateDefinition(definitionf(domainRef))
      }
    def removeDefinitionAction[_: P]: P[DomainRef => Action.RemoveDefinition] =
      P(remove ~/ name).map { name => (domainRef: DomainRef) =>
        Action.RemoveDefinition(DefinitionRef(domainRef, name))
      }
    def renameDefinitionAction[_: P]: P[DomainRef => Action.RenameDefinition] =
      P(rename ~/ name ~ name).map {
        case (oldName, newName) =>
          (domainRef: DomainRef) =>
            Action.RenameDefinition(DefinitionRef(domainRef, oldName), newName)
      }
    def addEnumValuesAction[_: P]: P[DomainRef => Action.AddEnumValues] =
      P(create ~/ enum ~ name ~ values ~ `(` ~ name.rep(sep = `,`) ~ `)`).map {
        case (name, newValues) =>
          (domainRef: DomainRef) =>
            Action.AddEnumValues(DefinitionRef(domainRef, name), newValues.to[ListSet])
      }
    def removeEnumValuesAction[_: P]: P[DomainRef => Action.RemoveEnumValues] =
      P(remove ~/ enum ~ name ~ values ~ `(` ~ name.rep(sep = `,`) ~ `)`).map {
        case (name, removedValues) =>
          (domainRef: DomainRef) =>
            Action.RemoveEnumValues(DefinitionRef(domainRef, name), removedValues.to[ListSet])
      }
    def renameEnumValuesAction[_: P]: P[DomainRef => Action.RenameEnumValues] =
      P(rename ~/ enum ~ name ~ values ~ `(` ~ (name ~ `->` ~ name).rep(sep = `,`) ~ `)`).map {
        case (name, renamedValues) =>
          (domainRef: DomainRef) =>
            Action.RenameEnumValues(DefinitionRef(domainRef, name), ListMap(renamedValues.toSeq: _*))
      }
    def addRecordFieldsAction[_: P]: P[DomainRef => Action.AddRecordFields] =
      P(create ~/ record ~ name ~ fields ~ `{` ~ (name ~ `->` ~ argument).rep(sep = `,`) ~ `}`).map {
        case (name, newFields) =>
          (domainRef: DomainRef) =>
            Action.AddRecordFields(DefinitionRef(domainRef, name), ListMap(newFields.map {
              case (k, vf) => k -> vf(domainRef)
            }.toSeq: _*))
      }
    def removeRecordFieldsAction[_: P]: P[DomainRef => Action.RemoveRecordFields] =
      P(remove ~/ record ~ name ~ fields ~ `{` ~ name.rep(sep = `,`) ~ `}`).map {
        case (name, removedFields) =>
          (domainRef: DomainRef) =>
            Action.RemoveRecordFields(DefinitionRef(domainRef, name), removedFields.to[ListSet])
      }
    def renameRecordFieldsAction[_: P]: P[DomainRef => Action.RenameRecordFields] =
      P(rename ~/ record ~ name ~ fields ~ `{` ~ (name ~ `->` ~ name).rep(sep = `,`) ~ `}`).map {
        case (name, renamedFields) =>
          (domainRef: DomainRef) =>
            Action.RenameRecordFields(DefinitionRef(domainRef, name), ListMap(renamedFields.toSeq: _*))
      }
    def action[_: P]: P[DomainRef => Action] =
      P(
        createDefinitionAction |
          removeDefinitionAction |
          renameDefinitionAction |
          addEnumValuesAction |
          removeEnumValuesAction |
          renameEnumValuesAction |
          addRecordFieldsAction |
          removeRecordFieldsAction |
          renameRecordFieldsAction
      )

    // migration

    def actionsInDomain[_: P]: P[List[Action]] =
      P(name ~ `{` ~/ action.rep ~ `}`).map {
        case (domainName, actionsf) =>
          val domainRef = DomainRef(domainName)
          actionsf.map(_(domainRef)).toList
      }

    def migration[_: P]: P[Migration] =
      P(actionsInDomain.rep ~ End).map { actionsLists =>
        Migration(actionsLists.toList.flatten)
      }
  }
}

@Cached class Parser[F[_]: Sync: SchemaErrorRaise] {

  import Parser._

  def apply(input: String): F[Migration] = Sync[F].delay(parse(input, NonTerminal.migration(_))).flatMap {
    case Success(value, _)    => value.pure[F]
    case Failure(label, a, b) => SchemaError.parsingError[F, Migration](s"$label: $a -> ${b}")
  }

  def apply(input: Iterator[String]): F[Migration] = Sync[F].delay(parse(input, NonTerminal.migration(_))).flatMap {
    case Success(value, _)    => value.pure[F]
    case Failure(label, _, _) => SchemaError.parsingError[F, Migration](label)
  }
}
