package dddblueprint
package compiler

import cats.implicits._
import dddblueprint.output.Dependencies
import monocle.macros.syntax.lens._

object DependencyResolver {

  val argToRef: output.Argument => ListSet[output.DefinitionRef] = {
    case ref: output.DefinitionRef  => ListSet(ref)
    case _:   output.Data.Primitive => ListSet.empty
    case output.Data.Collection.Option(of)      => argToRef(of)
    case output.Data.Collection.Array(of)       => argToRef(of)
    case output.Data.Collection.Set(of)         => argToRef(of)
    case output.Data.Collection.Map(key, value) => argToRef(key) ++ argToRef(value)
    case output.Data.Tuple(arguments)           => arguments.toListSet.flatMap(argToRef)
  }

  val argToNamedRef: (
    output.DefinitionRef => Option[String]
  ) => output.Argument => ListMap[String, output.DefinitionRef] = getName => {
    case ref: output.DefinitionRef  => ListMap(getName(ref).getOrElse("undefined") -> ref)
    case _:   output.Data.Primitive => ListMap.empty
    case output.Data.Collection.Option(of) =>
      argToNamedRef(getName)(of).map { case (name, ref) => show"$name?" -> ref }
    case output.Data.Collection.Array(of) =>
      argToNamedRef(getName)(of).map { case (name, ref) => show"[$name]" -> ref }
    case output.Data.Collection.Set(of) =>
      argToNamedRef(getName)(of).map { case (name, ref) => show"{$name}" -> ref }
    case output.Data.Collection.Map(key, value) =>
      argToNamedRef(getName)(key).map { case (name, ref) => show"{$name:_}" -> ref } ++ argToNamedRef(getName)(value)
        .map {
          case (name, ref) => show"{_:$name}" -> ref
        }
    case output.Data.Tuple(arguments) =>
      ListMap(
        (for {
          (arg, argIndex) <- arguments.zipWithIndex
          (name, ref) <- argToNamedRef(getName)(arg)
        } yield show"(${arguments.indices.map(i => if (i === argIndex) name else "_").mkString(",")})" -> ref).toSeq: _*
      )
  }

  val dataToDirectDependencies: output.Data => ListSet[output.DefinitionRef] = {
    case _: output.Data.Primitive | _: output.Data.Definition.Enum => ListSet.empty
    case collection: output.Data.Collection => argToRef(collection)
    case record:     output.Data.Definition.Record =>
      val output.Data.Definition.Record.Aux(_, fields, _) = record
      fields.values.toListSet.flatMap(argToRef(_).toList)
    case output.Data.Definition.Service(_, inputs, outputs) =>
      inputs.values.toListSet.flatMap(argToRef(_).toList) ++ outputs
    case output.Data.Definition.Publisher(_, events)  => events
    case output.Data.Definition.Subscriber(_, events) => events
  }

  val dataToDirectNamedDependencies: (
    output.DefinitionRef => Option[String]
  ) => output.Data => ListMap[String, output.DefinitionRef] = getName => {
    case _: output.Data.Primitive | _: output.Data.Definition.Enum => ListMap.empty
    case collection: output.Data.Collection => argToNamedRef(getName)(collection)
    case record:     output.Data.Definition.Record =>
      val output.Data.Definition.Record.Aux(_, fields, _) = record
      fields.map { case (field, ref) => field -> argToRef(ref) }.flatMap {
        case (field, refs) => refs.map(field -> _)
      }
    case output.Data.Definition.Service(_, inputs, outputs) =>
      // TODO: output # -> entity name
      inputs.map { case (field, ref) => field -> argToRef(ref) }.flatMap {
        case (field, refs) => refs.map(field -> _)
      } ++
        outputs.zipWithIndex.map { case (r, i) => show"output $i" -> r }
    case output.Data.Definition.Publisher(_, events) =>
      // TODO: event # -> entity name
      ListMap(events.zipWithIndex.map { case (r, i) => show"event $i" -> r }.toSeq: _*)
    case output.Data.Definition.Subscriber(_, events) =>
      // TODO: event # -> entity name
      ListMap(events.zipWithIndex.map { case (r, i) => show"event $i" -> r }.toSeq: _*)
  }

  def findTransitiveDependencies(
    definitions: ListMap[output.DefinitionRef, output.Data.Definition]
  ): ListMap[output.DefinitionRef, Dependencies] = {

    val refToDirectDependencies: output.DefinitionRef => ListSet[output.DefinitionRef] =
      definitions andThen dataToDirectDependencies

    @scala.annotation.tailrec
    def resolveRecur(
      resolved: ListMap[output.DefinitionRef, Dependencies],
      visited:  ListSet[output.DefinitionRef] = ListSet.empty
    ): ListMap[output.DefinitionRef, Dependencies] = {
      val candidates: ListSet[output.DefinitionRef] = resolved.values.toListSet.flatMap(_.transitive)

      val newlyResolved: Map[output.DefinitionRef, ListSet[output.DefinitionRef]] = (candidates -- visited).map { ref =>
        ref -> refToDirectDependencies(ref)
      }.toMap

      if (newlyResolved.isEmpty) {
        resolved
      } else {
        resolveRecur(
          resolved.map {
            case (ref, dependencies) =>
              ref -> dependencies.lens(_.transitive).modify { transitive =>
                transitive ++ transitive.flatMap(newlyResolved)
              }
          },
          visited ++ newlyResolved.keySet
        )
      }
    }

    resolveRecur(definitions.map {
      case (ref, body) =>
        ref -> output.Dependencies(dataToDirectDependencies(body))
    })
  }
}
