package dddblueprint
package compiler

import cats.implicits._
import cats.{ Monad, Traverse }
import io.scalaland.pulp.Cached

import scala.collection.immutable.ListSet

@Cached class ArgumentCompiler[F[_]: Monad: SnapshotState: SnapshotOperations] {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def apply(argument: input.Argument): F[output.Argument] =
    argument match {
      case ref: input.DefinitionRef => ref.translate[F].map(r => r: output.Argument)

      case p: input.Data.Primitive => (PrimitivesCompiler(p): output.Argument).pure[F]

      case input.Data.Collection.Option(of) => apply(of).map(output.Data.Collection.Option.apply)
      case input.Data.Collection.Array(of)  => apply(of).map(output.Data.Collection.Array.apply)
      case input.Data.Collection.Set(of)    => apply(of).map(output.Data.Collection.Set.apply)
      case input.Data.Collection.Map(key, value) =>
        for {
          k <- apply(key)
          v <- apply(value)
        } yield output.Data.Collection.Map(k, v)

      case input.Data.Tuple(arguments) =>
        Traverse[ListSet].sequence[F, output.Argument](arguments.map(apply)).map(output.Data.Tuple.apply)
    }
}

object ArgumentCompiler {

  @inline def apply[F[_]](implicit argumentCompiler: ArgumentCompiler[F]): ArgumentCompiler[F] = argumentCompiler
}
