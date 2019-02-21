package dddblueprint
package compiler

trait CompilerSpec extends FRunningSpec {

  abstract class TestSnapshot[A](result: StateIO[A])(base: output.Snapshot = output.Snapshot()) extends RefIso {
    private lazy val snapshotA = result.run(base).apply()
    def snapshot: output.Snapshot = snapshotA._1
    def a:        A               = snapshotA._2
  }
}
