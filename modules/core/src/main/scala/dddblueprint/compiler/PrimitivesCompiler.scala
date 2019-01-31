package dddblueprint
package compiler

import io.scalaland.chimney.dsl._

object PrimitivesCompiler {

  def apply(primitive: input.Data.Primitive): output.Data.Primitive =
    primitive.transformInto[output.Data.Primitive]

  def enumerable(enumerable: input.Data.Enumerable): output.Data.Enumerable =
    enumerable.transformInto[output.Data.Enumerable]
}
