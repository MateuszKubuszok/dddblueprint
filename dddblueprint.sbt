import sbt._
import Settings._

lazy val root = project.root
  .setName("dddblueprint")
  .setDescription("Build of dddblueprint")
  .configureRoot
  .aggregate(core, monix, laws)

lazy val core = project.from("core")
  .setName("dddblueprint-common")
  .setDescription("Schema for DDD projects")
  .setInitialImport("_")
  .configureModule
  .settings(Compile / resourceGenerators += task[Seq[File]] {
    val file = (Compile / resourceManaged).value / "dddblueprint-version.conf"
    IO.write(file, s"version=${version.value}")
    Seq(file)
  })

lazy val monix = project.from("monix")
  .setName("dddblueprint-monix")
  .setDescription("First project")
  .setInitialImport("monix._")
  .configureModule
  .compileAndTestDependsOn(core)
  .settings(
    libraryDependencies ++= Seq(Dependencies.monixEval, Dependencies.monixExecution)
  )

lazy val laws = project.from("laws")
  .setName("laws")
  .setDescription("dddblueprint-laws")
  .setInitialImport("laws._")
  .configureModule
  .configureTests(requiresFork = true)
  .dependsOn(core % "compile->compile", monix % "test->compile")

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
addCommandAlias("relock", ";unlock;reload;update;lock")
