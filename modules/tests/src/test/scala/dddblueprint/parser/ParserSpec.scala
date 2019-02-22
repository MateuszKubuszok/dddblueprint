package dddblueprint
package parser

import cats.implicits._
import debug.syntax._

trait ParserSpec extends FRunningSpec {

  abstract class TestParsing(result: IO[input.History]) {
    lazy val history = result.onErrorHandleWith {
      case ex @ SchemaError.Wrapper(errors) =>
        error"Parsing failed:\n$errors".withEx[IO](ex).flatMap(_ => ex.raiseError[IO, input.History])
      case ex: Throwable =>
        error"Parsing failed".withEx[IO](ex).flatMap(_ => ex.raiseError[IO, input.History])
    }()
  }
}
