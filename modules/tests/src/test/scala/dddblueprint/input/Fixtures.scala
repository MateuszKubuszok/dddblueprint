package dddblueprint
package input

import scala.collection.immutable.{ ListMap, ListSet }

object Fixtures {

  val Domain1Ref = input.DomainRef("test-1")
  val Domain2Ref = input.DomainRef("test-2")

  val Enum1Ref = input.DefinitionRef(Domain1Ref, "enum-1")
  val Enum2Ref = input.DefinitionRef(Domain2Ref, "enum-2")

  val Entity1Ref = input.DefinitionRef(Domain1Ref, "entity-1")
  val Entity2Ref = input.DefinitionRef(Domain2Ref, "entity-2")

  val Value1Ref = input.DefinitionRef(Domain1Ref, "value-1")
  val Value2Ref = input.DefinitionRef(Domain2Ref, "value-2")

  val Event1Ref = input.DefinitionRef(Domain1Ref, "event-1")
  val Event2Ref = input.DefinitionRef(Domain2Ref, "event-2")

  val Service1Ref = input.DefinitionRef(Domain1Ref, "service-1")
  val Service2Ref = input.DefinitionRef(Domain2Ref, "service-2")

  val Publisher1Ref = input.DefinitionRef(Domain1Ref, "publisher-1")
  val Publisher2Ref = input.DefinitionRef(Domain2Ref, "publisher-2")

  val Subscriber1Ref = input.DefinitionRef(Domain1Ref, "subscriber-1")
  val Subscriber2Ref = input.DefinitionRef(Domain2Ref, "subscriber-2")

  object Data {

    object Definition {

      val Enum1 = input.Data.Definition.Enum(Enum1Ref, ListSet("a", "b"), input.Data.String)
      val Enum2 = input.Data.Definition.Enum(Enum2Ref, ListSet("1", "2"), input.Data.Int)

      object Record {

        val Entity1 = input.Data.Definition.Record.Entity(Entity1Ref, ListMap.empty)
        val Entity2 = input.Data.Definition.Record.Entity(Entity2Ref, ListMap.empty)

        val Value1 = input.Data.Definition.Record.Value(Value1Ref, ListMap.empty)
        val Value2 = input.Data.Definition.Record.Value(Value2Ref, ListMap.empty)

        val Event1 = input.Data.Definition.Record.Event(Event1Ref, ListMap.empty)
        val Event2 = input.Data.Definition.Record.Event(Event2Ref, ListMap.empty)
      }

      val Service1 = input.Data.Definition.Service(Event1Ref, ListMap.empty, ListSet.empty)
      val Service2 = input.Data.Definition.Service(Event2Ref, ListMap.empty, ListSet.empty)

      val Publisher1 = input.Data.Definition.Publisher(Publisher1Ref, ListSet.empty)
      val Publisher2 = input.Data.Definition.Publisher(Publisher2Ref, ListSet.empty)

      val Subscriber1 = input.Data.Definition.Subscriber(Subscriber1Ref, ListSet.empty)
      val Subscriber2 = input.Data.Definition.Subscriber(Subscriber2Ref, ListSet.empty)
    }
  }
}
