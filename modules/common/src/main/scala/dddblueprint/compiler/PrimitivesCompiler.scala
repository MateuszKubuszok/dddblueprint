package dddblueprint
package compiler

import io.scalaland.chimney.dsl._

object PrimitivesCompiler {

  def apply(primitive: schema.Data.Primitive): validated.Data.Primitive =
    primitive.transformInto[validated.Data.Primitive]

  def enumerable(enumerable: schema.Data.Enumerable): validated.Data.Enumerable =
    enumerable.transformInto[validated.Data.Enumerable]
}
