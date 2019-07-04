package dddblueprint
package compiler

import cats.implicits._
import debug.syntax._

trait CompilerSpec extends FRunningSpec {

  abstract class TestSnapshot[A](result: StateIO[A])(base: output.Snapshot = output.Snapshot()) extends RefIso {
    private lazy val snapshotA = result
      .handleErrorWith {
        case ex @ SchemaError.Wrapper(errors) =>
          error"Parsing failed:\n$errors".withEx[StateIO](ex).flatMap(_ => ex.raiseError[StateIO, A]): StateIO[A]
        case ex: Throwable =>
          error"Parsing failed".withEx[StateIO](ex).flatMap(_ => ex.raiseError[StateIO, A]): StateIO[A]
      }
      .run(base)
      .apply()
    def snapshot: output.Snapshot = snapshotA._1
    def a:        A               = snapshotA._2
  }
}
