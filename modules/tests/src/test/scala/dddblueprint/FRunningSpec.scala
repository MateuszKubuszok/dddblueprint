package dddblueprint

import cats.effect.Sync
import dddblueprint.compiler.SnapshotState
import org.specs2.mutable.Specification

trait FRunningSpec extends Specification with logback.LogbackLogging.Module {

  val inputs  = input.Fixtures
  val outputs = output.Fixtures

  type StateIO[A] = monix.WithCoeval.CoevalState[A]
  type IO[A]      = _root_.monix.eval.Coeval[A]

  protected implicit val syncStateIO:              Sync[StateIO]              = FRunningSpec.implicits._1
  protected implicit val snapshotStateStateIO:     SnapshotState[StateIO]     = FRunningSpec.implicits._2
  protected implicit val schemaErrorHandleStateIO: SchemaErrorHandle[StateIO] = FRunningSpec.implicits._3
  protected implicit val syncIO:                   Sync[IO]                   = FRunningSpec.implicits._4
  protected implicit val schemaErrorHandleIO:      SchemaErrorHandle[IO]      = FRunningSpec.implicits._5
}

object FRunningSpec {

  type StateIO[A] = monix.WithCoeval.CoevalState[A]
  type IO[A]      = _root_.monix.eval.Coeval[A]

  private val implicits = locally {
    import cats.mtl.implicits._

    (Sync[StateIO],
     SnapshotState[StateIO],
     monix.WithCoeval.coevalStateSchemaErrorHandle,
     Sync[IO],
     monix.WithCoeval.coevalSchemaErrorHandle)
  }
}
