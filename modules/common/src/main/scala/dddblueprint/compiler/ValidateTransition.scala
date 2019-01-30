package dddblueprint
package compiler

object ValidateTransition extends ((validated.Snapshot, validated.Snapshot) => Result[validated.Snapshot]) {

  // TODO: removed definitions - ensure not used?
  // TODO: removed enum values - ensure not used? - add required migrations defs
  // TODO: removed records fields - ensure not used? - add required migrations defs
  def apply(oldVersion: validated.Snapshot, newVersion: validated.Snapshot): Result[validated.Snapshot] = ???
}
