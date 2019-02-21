package dddblueprint
package parser

trait ParserSpec extends FRunningSpec {

  abstract class TestParsing(result: IO[input.History]) {
    lazy val history = result()
  }
}
