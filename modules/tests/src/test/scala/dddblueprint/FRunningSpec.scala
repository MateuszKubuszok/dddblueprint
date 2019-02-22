package dddblueprint

import cats.data.StateT
import cats.effect.Sync
import com.typesafe.scalalogging.Logger
import dddblueprint.compiler.SnapshotState
import dddblueprint.debug.Logging
import monix.eval.Coeval
import org.specs2.mutable.Specification

trait FRunningSpec extends Specification with logback.LogbackLogging.Module {

  val inputs  = input.Fixtures
  val outputs = output.Fixtures

  type IO[A]      = FRunningSpec.IO[A]
  type StateIO[A] = FRunningSpec.StateIO[A]

  protected implicit val syncStateIO:          Sync[StateIO]          = FRunningSpec.implicits._1
  protected implicit val snapshotStateStateIO: SnapshotState[StateIO] = FRunningSpec.implicits._2
  protected implicit val loggingStateIO:       Logging[StateIO]       = FRunningSpec.implicits._3
  protected implicit val syncIO:               Sync[IO]               = FRunningSpec.implicits._4
  protected implicit val loggingIO:            Logging[IO]            = FRunningSpec.implicits._5
}

object FRunningSpec {

  type IO[A]      = Coeval[A]
  type StateIO[A] = StateT[IO, output.Snapshot, A]

  private val implicits = locally {
    import cats.mtl.implicits._

    val logger = Logger("tests")

    def loggingF[F[_]: Sync] = new debug.Logging[F] {
      def trace(msg: String) = Sync[F].delay(logger.trace(msg))
      def debug(msg: String) = Sync[F].delay(logger.debug(msg))
      def info(msg:  String) = Sync[F].delay(logger.info(msg))
      def warn(msg:  String) = Sync[F].delay(logger.warn(msg))
      def error(msg: String) = Sync[F].delay(logger.error(msg))
      def error(msg: String, ex: Throwable) = Sync[F].delay(logger.error(msg, ex))
    }

    (Sync[StateIO], SnapshotState[StateIO], loggingF[StateIO], Sync[IO], loggingF[IO])
  }
}
