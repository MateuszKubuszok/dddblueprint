package dddblueprint
package parser

import fastparse._
import input._
import ScalaWhitespace._

import scala.collection.immutable.ListSet

// scalastyle:off public.methods.have.type
// scalastyle:off method.name
@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "org.wartremover.warts.Var"))
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

    def domain[_: P]: P[Unit] = P(IgnoreCase("domain"))

    def create[_: P]: P[Unit] = P(IgnoreCase("create"))
    def remove[_: P]: P[Unit] = P(IgnoreCase("remove"))
    def rename[_: P]: P[Unit] = P(IgnoreCase("rename"))

    def enum[_:   P]: P[Unit] = P(IgnoreCase("enum"))
    def entity[_: P]: P[Unit] = P(IgnoreCase("entity"))
    def value[_:  P]: P[Unit] = P(IgnoreCase("value"))
    def event[_:  P]: P[Unit] = P(IgnoreCase("event"))

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
  }

  object NonTerminal {
    import Keyword._
    import Input._

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
      P(argument ~ `?`).map(_.andThen(Data.Collection.Option.apply))
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

    //def tupleDefinition[_: P] = P()

    def argument[_: P]: P[DomainRef => Argument] = ???

    def enumDefinition[_: P]: P[DomainRef => Data.Definition.Enum] =
      P(enum ~ of ~ enumerable ~ name ~ `(` ~ name.rep(sep = `,`) ~ `)`).map {
        case (etype, name, values) =>
          (currentDomain: DomainRef) =>
            Data.Definition.Enum(DefinitionRef(currentDomain, name), values.to[ListSet], etype)
      }
//    def entityDefinition[_: P] = P(entity ~ of ~ enumerable ~ name ~ `(` ~ name.rep(sep = `,`) ~ `)`).map {
//      case (etype, name, values) =>
//        (currentDomain: input.DomainRef) =>
//          input.Data.Definition.Enum(input.DefinitionRef(currentDomain, name), values.to[ListSet], etype)
//    }
  }

  def apply(input: String) = parse(input, Keyword.domain(_))
}
