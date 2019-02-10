package dddblueprint
package compiler

import monocle.macros.syntax.lens._

import scala.collection.immutable.{ ListMap, ListSet }

object DependencyResolver {

  val argumentToRef: output.Argument => Option[output.DefinitionRef] = {
    case ref: output.DefinitionRef  => Some(ref)
    case _:   output.Data.Primitive => None
    case output.Data.Definition.Record.Tuple(ref, _) => Some(ref)
  }

  val dataToDirectDependencies: output.Data => ListSet[output.DefinitionRef] = {
    case _: output.Data.Primitive | _: output.Data.Enumerable | _: output.Data.Definition.Enum => ListSet.empty
    case record: output.Data.Definition.Record =>
      val output.Data.Definition.Record.Aux(_, fields, _) = record
      fields.values.to[ListSet].flatMap(argumentToRef(_).toList)
    case output.Data.Definition.Service(_, inputs, outputs) =>
      inputs.values.to[ListSet].flatMap(argumentToRef(_).toList) ++ outputs
    case output.Data.Definition.Publisher(_, events)  => events
    case output.Data.Definition.Subscriber(_, events) => events
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
      val candidates: ListSet[output.DefinitionRef] = resolved.values.to[ListSet].flatMap(_.transitive)

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
        ref -> Dependencies(dataToDirectDependencies(body))
    })
  }
}
