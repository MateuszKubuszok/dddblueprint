package dddblueprint

import cats.data.StateT
import cats.effect.Sync
import dddblueprint.compiler.SnapshotState
import monix.eval.Coeval
import org.specs2.mutable.Specification

trait FRunningSpec extends Specification with logback.LogbackLogging.Module {

  val inputs  = input.Fixtures
  val outputs = output.Fixtures

  type IO[A]      = FRunningSpec.IO[A]
  type StateIO[A] = FRunningSpec.StateIO[A]

  protected implicit val syncStateIO:          Sync[StateIO]          = FRunningSpec.implicits._1
  protected implicit val snapshotStateStateIO: SnapshotState[StateIO] = FRunningSpec.implicits._2
  protected implicit val syncIO:               Sync[IO]               = FRunningSpec.implicits._3
}

object FRunningSpec {

  type IO[A]      = Coeval[A]
  type StateIO[A] = StateT[IO, output.Snapshot, A]

  private val implicits = locally {
    import cats.mtl.implicits._

    (Sync[StateIO], SnapshotState[StateIO], Sync[IO])
  }
}
