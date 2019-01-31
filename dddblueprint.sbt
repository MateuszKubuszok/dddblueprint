import sbt._
import Settings._

lazy val root = project.root
  .setName("dddblueprint")
  .setDescription("Build of dddblueprint")
  .configureRoot
  .aggregate(core, first, second)

lazy val core = project.from("core")
  .setName("dddblueprint-common")
  .setDescription("Schema for DDD projects")
  .setInitialImport("_")
  .configureModule
  .configureTests()
  .settings(Compile / resourceGenerators += task[Seq[File]] {
    val file = (Compile / resourceManaged).value / "dddblueprint-version.conf"
    IO.write(file, s"version=${version.value}")
    Seq(file)
  })

lazy val first = project.from("first")
  .setName("first")
  .setDescription("First project")
  .setInitialImport("first._")
  .configureModule
  .configureTests()
  .compileAndTestDependsOn(core)
  .configureRun("dddblueprint.first.First")

lazy val second = project.from("second")
  .setName("second")
  .setDescription("Second project")
  .setInitialImport("second._")
  .configureModule
  .configureTests()
  .compileAndTestDependsOn(core)
  .configureRun("dddblueprint.second.Second")

addCommandAlias("fullTest", ";test;fun:test;it:test;scalastyle")

addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")

addCommandAlias("relock", ";unlock;reload;update;lock")
