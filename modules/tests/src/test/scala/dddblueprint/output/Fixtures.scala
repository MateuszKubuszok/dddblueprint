package dddblueprint
package output

import scala.collection.immutable.{ ListMap, ListSet }

object Fixtures {

  val Domain1Ref = output.DomainRef()
  val Domain2Ref = output.DomainRef()

  val Enum1Ref = output.DefinitionRef()
  val Enum2Ref = output.DefinitionRef()

  val Entity1Ref = output.DefinitionRef()
  val Entity2Ref = output.DefinitionRef()

  val Value1Ref = output.DefinitionRef()
  val Value2Ref = output.DefinitionRef()

  val Event1Ref = output.DefinitionRef()
  val Event2Ref = output.DefinitionRef()

  val Service1Ref = output.DefinitionRef()
  val Service2Ref = output.DefinitionRef()

  val Publisher1Ref = output.DefinitionRef()
  val Publisher2Ref = output.DefinitionRef()

  val Subscriber1Ref = output.DefinitionRef()
  val Subscriber2Ref = output.DefinitionRef()

  object Data {

    object Definition {

      val Enum1 = output.Data.Definition.Enum(Enum1Ref, ListSet("a", "b"), output.Data.String)
      val Enum2 = output.Data.Definition.Enum(Enum2Ref, ListSet("1", "2"), output.Data.Int)

      object Record {

        val Entity1 = output.Data.Definition.Record.Entity(Entity1Ref, ListMap.empty)
        val Entity2 = output.Data.Definition.Record.Entity(Entity2Ref, ListMap.empty)

        val Value1 = output.Data.Definition.Record.Value(Value1Ref, ListMap.empty)
        val Value2 = output.Data.Definition.Record.Value(Value2Ref, ListMap.empty)

        val Event1 = output.Data.Definition.Record.Event(Event1Ref, ListMap.empty)
        val Event2 = output.Data.Definition.Record.Event(Event2Ref, ListMap.empty)
      }

      val Service1 = output.Data.Definition.Service(Event1Ref, ListMap.empty, ListSet.empty)
      val Service2 = output.Data.Definition.Service(Event2Ref, ListMap.empty, ListSet.empty)

      val Publisher1 = output.Data.Definition.Publisher(Publisher1Ref, ListSet.empty)
      val Publisher2 = output.Data.Definition.Publisher(Publisher2Ref, ListSet.empty)

      val Subscriber1 = output.Data.Definition.Subscriber(Subscriber1Ref, ListSet.empty)
      val Subscriber2 = output.Data.Definition.Subscriber(Subscriber2Ref, ListSet.empty)
    }
  }
}
