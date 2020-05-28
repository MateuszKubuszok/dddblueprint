package dddblueprint

import java.io.File

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.Sync
import dddblueprint.compiler.{ FixedMigrationCompiler, HistoryCompiler, MigrationCompiler, SnapshotState }
import dddblueprint.debug.Logging
import dddblueprint.debug.syntax._
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
    for {
      _ <- info"""Resolved relatively to ${new File(".").getAbsolutePath}""".apply[IO]
      _ <- args.toList.traverse { directory =>
        (for {
          _ <- info"""Parsing data from $directory""".apply[IO]
          history <- DirectoryParser(directory)
          _ <- info"""Input history:\n$history""".apply[IO]
          blueprint <- HistoryCompiler(history)
          _ <- info"""Output blueprint:\n$blueprint""".apply[IO]
        } yield ()).handleWith[NonEmptyList[SchemaError]] {
          _.toList.traverse(e => error"""$e""".apply[IO]).void
        }
      }
    } yield ()
  }.value
}
