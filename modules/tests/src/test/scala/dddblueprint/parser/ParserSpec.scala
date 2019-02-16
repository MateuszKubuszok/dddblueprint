package dddblueprint
package parser

trait ParserSpec extends FRunningSpec {

  abstract class TestParsing(result: F[input.History]) {
    lazy val history = result.runA(output.Snapshot()).apply()
  }
}
