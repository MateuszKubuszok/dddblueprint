package dddblueprint

import java.io.File

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.Sync
import dddblueprint.compiler.{ FixedMigrationCompiler, HistoryCompiler, MigrationCompiler, SnapshotState }
import dddblueprint.debug.Logging
import dddblueprint.parser.DirectoryParser
import io.scalaland.pulp._

object Main {

  type IO[A]      = FRunningSpec.IO[A]
  type StateIO[A] = FRunningSpec.StateIO[A]

  implicit val syncStateIO:          Sync[StateIO]          = FRunningSpec.implicits._1
  implicit val snapshotStateStateIO: SnapshotState[StateIO] = FRunningSpec.implicits._2
  implicit val loggingStateIO:       Logging[StateIO]       = FRunningSpec.implicits._3
  implicit val syncIO:               Sync[IO]               = FRunningSpec.implicits._4
  implicit val loggingIO:            Logging[IO]            = FRunningSpec.implicits._5

  implicit def migrationProvider: Provider[FixedMigrationCompiler[IO]] =
    Provider.upcast[MigrationCompiler[StateIO, IO], FixedMigrationCompiler[IO]]

  val DirectoryParser = Provider.get[DirectoryParser[IO]]
  val HistoryCompiler = Provider.get[HistoryCompiler[IO]]

  def main(args: Array[String]): Unit = {
    import cats.mtl.implicits._
    (for {
      _ <- Logging[IO].info(s"Resolved relatively to ${new File(".").getAbsolutePath}")
      _ <- args.toList.traverse { directory =>
        (for {
          _ <- Logging[IO].info(directory)
          history <- DirectoryParser(directory)
          _ <- Logging[IO].info(history.show)
          blueprint <- HistoryCompiler(history)
          _ <- Logging[IO].info(blueprint.show)
        } yield ()).handleWith[NonEmptyList[SchemaError]] { errors =>
          errors.toList.traverse(error => Logging[IO].error(error.show)).void
        }
      }
    } yield ()).value
  }
}
