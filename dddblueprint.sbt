import sbt._
import Settings._

lazy val root = project.root
  .setName("dddblueprint")
  .setDescription("Build of dddblueprint")
  .configureRoot
  .aggregate(core, parser, logback, tests)

lazy val core = project.from("core")
  .setName("dddblueprint-core")
  .setDescription("Schema for DDD projects")
  .setInitialImport("dddblueprint.compiler._")
  .configureModule
  .settings(Compile / resourceGenerators += task[Seq[File]] {
    val file = (Compile / resourceManaged).value / "dddblueprint-version.conf"
    IO.write(file, s"version=${version.value}")
    Seq(file)
  })

lazy val parser = project.from("parser")
  .setName("dddblueprint-parser")
  .setDescription("Parser for ddd blueprint schema")
  .setInitialImport("dddblueprint.parser._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.fastparse)
  )

lazy val logback = project.from("logback")
  .setName("dddblueprint-logback")
  .setDescription("Lockback type classes to log")
  .setInitialImport("dddblueprint.logback._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.scalaLogging, Dependencies.logback)
  )

lazy val tests = project.from("tests")
  .setName("tests")
  .setDescription("dddblueprint-tests")
  .configureModule
  .configureTests(requiresFork = true)
  .dependsOn(core % "test->compile", parser % "test->compile", logback % "test->compile")
  .settings(
    libraryDependencies ++= Seq(Dependencies.monixEval, Dependencies.monixExecution)
  )

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
addCommandAlias("relock", ";unlock;reload;update;lock")
