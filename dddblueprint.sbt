import sbt._
import Settings._

lazy val root = project.root
  .setName("dddblueprint")
  .setDescription("Build of dddblueprint")
  .configureRoot
  .aggregate(core, parser, logback, monix, tests)

lazy val core = project.from("core")
  .setName("dddblueprint-common")
  .setDescription("Schema for DDD projects")
  .setInitialImport("compiler._")
  .configureModule
  .settings(Compile / resourceGenerators += task[Seq[File]] {
    val file = (Compile / resourceManaged).value / "dddblueprint-version.conf"
    IO.write(file, s"version=${version.value}")
    Seq(file)
  })

lazy val parser = project.from("parser")
  .setName("dddblueprint-parser")
  .setDescription("Parser for ddd blueprint schema")
  .setInitialImport("parser._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.fastparse)
  )

lazy val logback = project.from("logback")
  .setName("dddblueprint-logback")
  .setDescription("Lockback type classes to log")
  .setInitialImport("logback._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.scalaLogging, Dependencies.logback)
  )

lazy val monix = project.from("monix")
  .setName("dddblueprint-monix")
  .setDescription("Monix type classes to run compilation")
  .setInitialImport("monix._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.monixEval, Dependencies.monixExecution)
  )

lazy val tests = project.from("tests")
  .setName("tests")
  .setDescription("dddblueprint-tests")
  .configureModule
  .configureTests(requiresFork = true)
  .dependsOn(core % "test->compile", parser % "test->compile", logback % "test->compile", monix % "test->compile")

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
addCommandAlias("relock", ";unlock;reload;update;lock")
