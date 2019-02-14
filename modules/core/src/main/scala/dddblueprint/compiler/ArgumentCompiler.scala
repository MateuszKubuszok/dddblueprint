package dddblueprint
package compiler

import cats.implicits._
import cats.Monad
import io.scalaland.pulp.Cached

@Cached class ArgumentCompiler[F[_]: Monad: SnapshotState: SnapshotOperations] {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def apply(argument: input.Argument, createTuple: input.Data.Definition.Record.Tuple => F[Unit]): F[output.Argument] =
    argument match {
      case ref: input.DefinitionRef => ref.translate[F].map(r => r: output.Argument)

      case p: input.Data.Primitive => (PrimitivesCompiler(p): output.Argument).pure[F]

      case input.Data.Collection.Option(of) => apply(of, createTuple).map(output.Data.Collection.Option.apply)
      case input.Data.Collection.Array(of)  => apply(of, createTuple).map(output.Data.Collection.Array.apply)
      case input.Data.Collection.Set(of)    => apply(of, createTuple).map(output.Data.Collection.Set.apply)
      case input.Data.Collection.Map(key, value) =>
        for {
          k <- apply(key, createTuple)
          v <- apply(value, createTuple)
        } yield output.Data.Collection.Map(k, v)

      case tuple @ input.Data.Definition.Record.Tuple(ref, _) =>
        for {
          internalRef <- ref.requireNotExisted[F]
          _ <- createTuple(tuple)
        } yield internalRef: output.Argument
    }
}

object ArgumentCompiler {

  @inline def apply[F[_]](implicit argumentCompiler: ArgumentCompiler[F]): ArgumentCompiler[F] = argumentCompiler
}
