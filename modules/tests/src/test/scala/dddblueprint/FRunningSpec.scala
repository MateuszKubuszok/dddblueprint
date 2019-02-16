package dddblueprint

import cats.effect.Sync
import dddblueprint.compiler.SnapshotState
import org.specs2.mutable.Specification

trait FRunningSpec extends Specification with logback.LogbackLogging.Module {

  val inputs  = input.Fixtures
  val outputs = output.Fixtures

  type F[A] = monix.WithCoeval.CoevalState[A]

  protected implicit val sync:              Sync[F]              = FRunningSpec.implicits._1
  protected implicit val snapshotState:     SnapshotState[F]     = FRunningSpec.implicits._2
  protected implicit val schemaErrorHandle: SchemaErrorHandle[F] = FRunningSpec.implicits._3
}

object FRunningSpec {

  type F[A] = monix.WithCoeval.CoevalState[A]

  private val implicits = locally {
    import cats.mtl.implicits._

    (Sync[F], SnapshotState[F], monix.WithCoeval.taskStateSchemaErrorHandle)
  }
}
