package dddblueprint
package backend

sealed trait Rendering[-Ctx, +A] extends ADT
object Rendering {

  sealed trait ArgumentR[Ctx, A] extends Rendering[Ctx, A]
  final case class DefinitionRefR[Ctx, A](context: Ctx, ref:       A) extends ArgumentR[Ctx, A]
  final case class PrimitiveR[Ctx, A](context:     Ctx, primitive: A) extends ArgumentR[Ctx, A]
  sealed trait CollectionR[Ctx, A] extends ArgumentR[Ctx, A]
  object CollectionR {
    final case class OptionR[Ctx, A](context: Ctx, of: A) extends CollectionR[Ctx, A]
  }
}
