package dddblueprint
package schema

final case class Migration(actions: List[Action]) extends ADT
