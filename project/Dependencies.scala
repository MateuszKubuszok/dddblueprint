import sbt._

import Dependencies._

object Dependencies {

  // scala version
  val scalaOrganization = "org.scala-lang"
  val scalaVersion      = "2.12.8"

  // build tools version
  val scalaFmtVersion = "1.5.1"

  // libraries versions
  val catsVersion       = "1.6.0"
  val catsEffectVersion = "1.2.0"
  val monixVersion      = "3.0.0-RC2"
  val monocleVersion    = "1.6.0-M1"
  val specs2Version     = "4.4.1"

  // resolvers
  val resolvers = Seq(
    Resolver sonatypeRepo "public",
    Resolver typesafeRepo "releases"
  )

  // functional libraries
  val cats               = "org.typelevel"                %% "cats-core"                 % catsVersion
  val catsEffect         = "org.typelevel"                %% "cats-effect"               % catsEffectVersion
  val catsLaws           = "org.typelevel"                %% "cats-laws"                 % catsVersion
  val catsMTL            = "org.typelevel"                %% "cats-mtl-core"             % "0.4.0"
  val catnip             = "io.scalaland"                 %% "catnip"                    % "0.5.1"
  val chimney            = "io.scalaland"                 %% "chimney"                   % "0.3.0"
  val pulp               = "io.scalaland"                 %% "pulp"                      % "0.0.8"
  val matrioshka         = "com.slamdata"                 %% "matryoshka-core"           % "0.18.3"
  val monocle            = "com.github.julien-truffaut"   %% "monocle-core"              % monocleVersion
  val monocleMacro       = "com.github.julien-truffaut"   %% "monocle-macro"             % monocleVersion
  val shapeless          = "com.chuusai"                  %% "shapeless"                 % "2.3.3"
  // async
  val monixExecution     = "io.monix"                     %% "monix-execution"           % monixVersion
  val monixEval          = "io.monix"                     %% "monix-eval"                % monixVersion
  // config
  val scopt              = "com.github.scopt"             %% "scopt"                     % "3.7.0"
  val scalaConfig        = "com.typesafe"                 %  "config"                    % "1.3.3"
  val pureConfig         = "com.github.pureconfig"        %% "pureconfig"                % "0.9.2"  excludeAll (
          ExclusionRule(   "org.scala-lang")
  )
  // generating
  val scalameta          = "org.scalameta"                %% "scalameta"                 % "4.1.0"
  // parsing
  val fastparse          = "com.lihaoyi"                  %% "fastparse"                 % "2.1.0"
  // logging
  val scalaLogging       = "com.typesafe.scala-logging"   %% "scala-logging"             % "3.9.0"
  val logback            = "ch.qos.logback"               %  "logback-classic"           % "1.2.3"
  // testing
  val spec2Core          = "org.specs2"                   %% "specs2-core"               % specs2Version
  val spec2Mock          = "org.specs2"                   %% "specs2-mock"               % specs2Version
  val spec2Scalacheck    = "org.specs2"                   %% "specs2-scalacheck"         % specs2Version
}

trait Dependencies {

  val scalaOrganizationUsed = scalaOrganization
  val scalaVersionUsed = scalaVersion

  val scalaFmtVersionUsed = scalaFmtVersion

  // resolvers
  val commonResolvers = resolvers

  val mainDeps = Seq(
    // functional libraries
    cats, catsEffect, catsMTL, catnip, chimney, monocle, monocleMacro, pulp, shapeless,
    // config
    scopt, scalaConfig, pureConfig
  )

  val testDeps = Seq(catsLaws, spec2Core, spec2Mock, spec2Scalacheck)

  implicit final class ProjectRoot(project: Project) {

    def root: Project = project in file(".")
  }

  implicit final class ProjectFrom(project: Project) {

    private val commonDir = "modules"

    def from(dir: String): Project = project in file(s"$commonDir/$dir")
  }

  implicit final class DependsOnProject(project: Project) {

    private val testConfigurations = Set("test", "fun", "it")
    private def findCompileAndTestConfigs(p: Project) =
      (p.configurations.map(_.name).toSet intersect testConfigurations) + "compile"

    private val thisProjectsConfigs = findCompileAndTestConfigs(project)
    private def generateDepsForProject(p: Project) =
      p % (thisProjectsConfigs intersect findCompileAndTestConfigs(p) map (c => s"$c->$c") mkString ";")

    def compileAndTestDependsOn(projects: Project*): Project =
      project dependsOn (projects.map(generateDepsForProject): _*)
  }
}
