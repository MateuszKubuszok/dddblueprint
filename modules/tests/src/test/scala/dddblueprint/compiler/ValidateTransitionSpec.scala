package dddblueprint
package compiler

import cats.data.NonEmptyList
import io.scalaland.pulp.Provider
import org.specs2.specification.Scope

class ValidateTransitionSpec extends CompilerSpec {

  "ValidateTransition" should {

    val domainRef   = outputs.Domain1Ref
    val domainName  = inputs.Domain1Ref.name
    val domain2Ref  = outputs.Domain2Ref
    val domain2Name = inputs.Domain2Ref.name

    "pass if none of removed definitions is used (all definitions exist)" in new Fixture {
      val ref1        = outputs.Enum1Ref
      val ref2        = outputs.Value1Ref
      val name1       = inputs.Enum1Ref.name
      val name2       = inputs.Value1Ref.name
      val definition1 = outputs.Data.Definition.Enum1
      val definition2 = outputs.Data.Definition.Record.Value1.withFields(ListMap("a" -> ref1))

      val oldSnapshot = output.Snapshot().withDomainRef(domainRef, domainName)
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        // check didn't erred
        assert(snapshot.definitions.keySet.contains(ref1) && snapshot.definitions.keySet.contains(ref2))
      }
    }

    "fail if some of removed definitions is used (not all definitions exist)" in new Fixture {
      val ref1        = outputs.Enum1Ref
      val ref2        = outputs.Value1Ref
      val name2       = inputs.Value1Ref.name
      val definition2 = outputs.Data.Definition.Record.Value1.withFields(ListMap("a" -> ref1))

      val oldSnapshot = output.Snapshot().withDomainRef(domainRef, domainName)
      val newSnapshot = oldSnapshot.bumpVersion.withDefinition(domainRef, domainName, ref2, name2, definition2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(NonEmptyList.of(SchemaError.FieldDefinitionMissing(domainName, name2, "a")))
        )
      }
    }

    "pass if types matches between versions" in new Fixture {
      val ref1        = outputs.Enum1Ref
      val ref2        = outputs.Value1Ref
      val name1       = inputs.Enum1Ref.name
      val name2       = inputs.Value1Ref.name
      val definition1 = outputs.Data.Definition.Enum1
      val definition2 = outputs.Data.Definition.Record.Value1.withFields(ListMap("a" -> ref1))

      val oldSnapshot = output
        .Snapshot()
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, ref1, definition1.withValues(ListSet("new")))
        .withDefinition(domainRef, ref2, definition2.withFields(ListMap("b" -> ref1)))

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        // check didn't erred
        assert(snapshot.definitions.keySet.contains(ref1) && snapshot.definitions.keySet.contains(ref2))
      }
    }

    "fail if types doesn't match between versions" in new Fixture {
      val ref1        = outputs.Enum1Ref
      val ref2        = outputs.Value1Ref
      val name1       = inputs.Enum1Ref.name
      val name2       = inputs.Value1Ref.name
      val definition1 = outputs.Data.Definition.Enum1
      val definition2 = outputs.Data.Definition.Record.Value1.withFields(ListMap("a" -> ref1))

      val oldSnapshot = output
        .Snapshot()
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, ref1, definition2)
        .withDefinition(domainRef, ref2, definition1)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(domainName, name1, "enum", definition2),
              SchemaError.DefinitionTypeMismatch(domainName, name2, "record", definition1)
            )
          )
        )
      }
    }

    "fail if event is published outside its domain" in new Fixture {
      val ref1        = outputs.Event1Ref
      val ref2        = outputs.Publisher2Ref
      val name1       = inputs.Event1Ref.name
      val name2       = inputs.Publisher2Ref.name
      val definition1 = outputs.Data.Definition.Record.Event1
      val definition2 = outputs.Data.Definition.Publisher2.copy(events = ListSet(ref1))

      val oldSnapshot = output.Snapshot()
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domain2Ref, domain2Name, ref2, name2, definition2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.EventPublishedOutsideDomain(domainName, name1, definition2)
            )
          )
        )
      }
    }

    "fail if publisher should publish non-event" in new Fixture {
      val ref1        = outputs.Entity1Ref
      val ref2        = outputs.Publisher1Ref
      val name1       = inputs.Entity1Ref.name
      val name2       = inputs.Publisher1Ref.name
      val definition1 = outputs.Data.Definition.Record.Entity1
      val definition2 = outputs.Data.Definition.Publisher1.copy(events = ListSet(ref1))

      val oldSnapshot = output.Snapshot()
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(domainName, name2, "event", definition1)
            )
          )
        )
      }
    }

    "fail if subscriber should subscribe to non-event" in new Fixture {
      val ref1        = outputs.Entity1Ref
      val ref2        = outputs.Subscriber1Ref
      val name1       = inputs.Entity1Ref.name
      val name2       = inputs.Subscriber1Ref.name
      val definition1 = outputs.Data.Definition.Record.Entity1
      val definition2 = outputs.Data.Definition.Subscriber1.copy(events = ListSet(ref1))

      val oldSnapshot = output.Snapshot()
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot must throwA(
          SchemaError.Wrapper(
            NonEmptyList.of(
              SchemaError.DefinitionTypeMismatch(domainName, name2, "event", definition1)
            )
          )
        )
      }
    }

    "calculate migrations for changed enum definitions" in new Fixture {
      val ref1        = outputs.Enum1Ref
      val ref2        = outputs.Enum2Ref
      val ref3        = outputs.Value1Ref
      val ref4        = outputs.Service1Ref
      val name1       = inputs.Enum1Ref.name
      val name2       = inputs.Enum2Ref.name
      val name3       = inputs.Value1Ref.name
      val name4       = inputs.Service1Ref.name
      val definition1 = outputs.Data.Definition.Enum1.withValues(ListSet("a", "b"))
      val definition2 = outputs.Data.Definition.Enum2.withValues(ListSet("a", "b"))
      val definition3 = outputs.Data.Definition.Record.Value1.withFields(ListMap("a" -> ref1, "b" -> ref2))
      val definition4 =
        outputs.Data.Definition.Service1.copy(input = ListMap("a" -> ref1, "b" -> ref2), output = ListSet(ref1, ref2))

      val oldSnapshot = output
        .Snapshot()
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)
        .withDefinition(domainRef, domainName, ref3, name3, definition3)
        .withDefinition(domainRef, domainName, ref4, name4, definition4)
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, ref1, definition1.withValues(ListSet("a", "b", "c")))
        .withDefinition(domainRef, ref2, definition2.withValues(ListSet("a")))

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot.automaticMigrations === ListMap(
          ref3 -> output.Dependencies(ListSet(ref1)),
          ref4 -> output.Dependencies(ListSet(ref1)),
          ref1 -> output.Dependencies(ListSet(ref1))
        )
        snapshot.manualMigrations === ListMap(
          ref3 -> output.Dependencies(ListSet(ref2, ref3)),
          ref4 -> output.Dependencies(ListSet(ref2, ref4)),
          ref2 -> output.Dependencies(ListSet(ref2))
        )
      }
    }

    "calculate migrations for changed record definitions" in new Fixture {
      val ref1        = outputs.Entity1Ref
      val ref2        = outputs.Value1Ref
      val ref3        = outputs.Event1Ref
      val ref4        = outputs.Service1Ref
      val ref5        = outputs.Publisher1Ref
      val ref6        = outputs.Subscriber1Ref
      val name1       = inputs.Entity1Ref.name
      val name2       = inputs.Value1Ref.name
      val name3       = inputs.Event1Ref.name
      val name4       = inputs.Service1Ref.name
      val name5       = inputs.Publisher1Ref.name
      val name6       = inputs.Subscriber1Ref.name
      val definition1 = outputs.Data.Definition.Record.Entity1.withFields(ListMap("a" -> output.Data.Int))
      val definition2 = outputs.Data.Definition.Record.Value1
      val definition3 = outputs.Data.Definition.Record.Event1.withFields(ListMap("a" -> ref1, "b" -> ref2))
      val definition4 = outputs.Data.Definition.Service1
        .copy(input = ListMap("a" -> ref1, "b" -> ref2), output = ListSet(ref1, ref2, ref3))
      val definition5 = outputs.Data.Definition.Publisher1.copy(events  = ListSet(ref3))
      val definition6 = outputs.Data.Definition.Subscriber1.copy(events = ListSet(ref3))

      val oldSnapshot = output
        .Snapshot()
        .withDefinition(domainRef, domainName, ref1, name1, definition1)
        .withDefinition(domainRef, domainName, ref2, name2, definition2)
        .withDefinition(domainRef, domainName, ref3, name3, definition3)
        .withDefinition(domainRef, domainName, ref4, name4, definition4)
        .withDefinition(domainRef, domainName, ref5, name5, definition5)
        .withDefinition(domainRef, domainName, ref6, name6, definition6)
      val newSnapshot = oldSnapshot.bumpVersion
        .withDefinition(domainRef, ref1, definition1.withFields(ListMap("b" -> output.Data.Int)))
        .withoutDefinition(domainRef, ref2)

      new TestSnapshot(ValidateTransition(oldSnapshot, newSnapshot))(newSnapshot) {
        snapshot.automaticMigrations === ListMap()
        snapshot.manualMigrations === ListMap(
          ref1 -> output.Dependencies(ListSet(ref1)),
          ref3 -> output.Dependencies(ListSet(ref3, ref1)),
          ref4 -> output.Dependencies(ListSet(ref1, ref3, ref4)),
          ref5 -> output.Dependencies(ListSet(ref3, ref5), ListSet(ref3, ref1, ref5)),
          ref6 -> output.Dependencies(ListSet(ref3, ref6), ListSet(ref3, ref1, ref6))
        )
      }
    }
  }

  private trait Fixture extends Scope {
    val ValidateTransition = Provider.get[ValidateTransition[StateIO]]
  }
}
