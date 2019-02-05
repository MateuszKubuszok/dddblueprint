package dddblueprint
package compiler

import cats.effect.Sync
import org.specs2.mutable.Specification

trait CompilerSpec extends Specification {

  type F[A] = monix.WithCoeval.CoevalState[A]

  protected implicit val sync:              Sync[F]              = CompilerSpec.implicits._1
  protected implicit val snapshotState:     SnapshotState[F]     = CompilerSpec.implicits._2
  protected implicit val schemaErrorHandle: SchemaErrorHandle[F] = CompilerSpec.implicits._3

  abstract class TestSnapshot[A](result: F[A])(base: output.Snapshot = output.Snapshot()) extends {
    val (snapshot, a) = result.run(base).apply()
  } with RefIso(snapshot)
}

object CompilerSpec {

  type F[A] = monix.WithCoeval.CoevalState[A]

  private val implicits = locally {
    import cats.mtl.implicits._

    (Sync[F], SnapshotState[F], monix.WithCoeval.taskStateSchemaErrorHandle)
  }
}
