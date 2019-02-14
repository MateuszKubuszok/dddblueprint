package dddblueprint
package compiler

import monocle.macros.syntax.lens._

import scala.collection.immutable.{ ListMap, ListSet }

object DependencyResolver {

  val argumentToRef: output.Argument => ListSet[output.DefinitionRef] = {
    case ref: output.DefinitionRef  => ListSet(ref)
    case _:   output.Data.Primitive => ListSet.empty
    case output.Data.Collection.Option(of)           => argumentToRef(of)
    case output.Data.Collection.Array(of)            => argumentToRef(of)
    case output.Data.Collection.Set(of)              => argumentToRef(of)
    case output.Data.Collection.Map(key, value)      => argumentToRef(key) ++ argumentToRef(value)
    case output.Data.Definition.Record.Tuple(ref, _) => ListSet(ref)
  }

  val dataToDirectDependencies: output.Data => ListSet[output.DefinitionRef] = {
    case _: output.Data.Primitive | _: output.Data.Definition.Enum => ListSet.empty
    case collection: output.Data.Collection => argumentToRef(collection)
    case record:     output.Data.Definition.Record =>
      val output.Data.Definition.Record.Aux(_, fields, _) = record
      fields.values.to[ListSet].flatMap(argumentToRef(_).toList)
    case output.Data.Definition.Service(_, inputs, outputs) =>
      inputs.values.to[ListSet].flatMap(argumentToRef(_).toList) ++ outputs
    case output.Data.Definition.Publisher(_, events)  => events
    case output.Data.Definition.Subscriber(_, events) => events
  }

  val dataToDirectNamedDependencies: output.Data => ListMap[String, output.DefinitionRef] = {
    case _: output.Data.Primitive | _: output.Data.Definition.Enum => ListMap.empty
    case output.Data.Collection.Option(of) =>
      output.Argument.toDataOrRef(of) match {
        case Left(data) => dataToDirectNamedDependencies(data).map { case (name, ref) => s"option[$name]" -> ref }
        case Right(ref) => ListMap("option[?]" -> ref) // TODO: name
      }
    case output.Data.Collection.Array(of) =>
      output.Argument.toDataOrRef(of) match {
        case Left(data) => dataToDirectNamedDependencies(data).map { case (name, ref) => s"array[$name]" -> ref }
        case Right(ref) => ListMap("array[?]" -> ref) // TODO: name
      }
    case output.Data.Collection.Set(of) =>
      output.Argument.toDataOrRef(of) match {
        case Left(data) => dataToDirectNamedDependencies(data).map { case (name, ref) => s"set[$name]" -> ref }
        case Right(ref) => ListMap("set[?]" -> ref) // TODO: name
      }
    case output.Data.Collection.Map(key, value) =>
      (output.Argument.toDataOrRef(key) match {
        case Left(data) => dataToDirectNamedDependencies(data).map { case (name, ref) => s"mapKey[$name]" -> ref }
        case Right(ref) => ListMap("mapKey[?]" -> ref) // TODO: name
      }) ++ (output.Argument.toDataOrRef(value) match {
        case Left(data) => dataToDirectNamedDependencies(data).map { case (name, ref) => s"mapValue[$name]" -> ref }
        case Right(ref) => ListMap("mapValue[?]" -> ref) // TODO: name
      })
    case record: output.Data.Definition.Record =>
      val output.Data.Definition.Record.Aux(_, fields, _) = record
      fields.map { case (field, ref) => field -> argumentToRef(ref) }.flatMap {
        case (field, refs) => refs.map(field -> _)
      }
    case output.Data.Definition.Service(_, inputs, outputs) =>
      // TODO: output # -> entity name
      inputs.map { case (field, ref) => field -> argumentToRef(ref) }.flatMap {
        case (field, refs) => refs.map(field -> _)
      } ++
        outputs.zipWithIndex.map { case (r, i) => s"output $i" -> r }
    case output.Data.Definition.Publisher(_, events) =>
      // TODO: event # -> entity name
      ListMap(events.zipWithIndex.map { case (r, i) => s"event $i" -> r }.toSeq: _*)
    case output.Data.Definition.Subscriber(_, events) =>
      // TODO: event # -> entity name
      ListMap(events.zipWithIndex.map { case (r, i) => s"event $i" -> r }.toSeq: _*)
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
