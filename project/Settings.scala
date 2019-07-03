import sbt._
import sbt.Keys._
import sbt.TestFrameworks.Specs2
import sbt.Tests.Argument
import com.typesafe.sbt._
import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._
import org.scalastyle.sbt.ScalastylePlugin.autoImport._
import sbtassembly.AssemblyPlugin.autoImport._
import scoverage._
import spray.revolver.RevolverPlugin.autoImport._
import wartremover._

object Settings extends Dependencies {

  val FunctionalTest: Configuration = config("fun") extend Test describedAs "Runs only functional tests"

  private val commonSettings = Seq(
    organization := "com.kubuszok",
    scalaOrganization := scalaOrganizationUsed,
    scalaVersion := scalaVersionUsed,
    scalafmtVersion := scalaFmtVersionUsed
  )

  private val rootSettings = commonSettings

  private val modulesSettings = commonSettings ++ Seq(
    scalacOptions := Seq(
      // standard settings
      "-target:jvm-1.8",
      "-encoding",
      "UTF-8",
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      // language features
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      // private options
      "-Ybackend-parallelism",
      "8",
      "-Yno-adapted-args",
      "-Ypartial-unification",
      // warnings
      "-Ywarn-dead-code",
      "-Ywarn-extra-implicit",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-macros:after",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:implicits",
      "-Ywarn-unused:imports",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates",
      "-Ywarn-value-discard",
      // advanced options
      "-Xcheckinit",
      "-Xfatal-warnings",
      "-Xfuture",
      // linting
      "-Xlint",
      "-Xlint:adapted-args",
      "-Xlint:by-name-right-associative",
      "-Xlint:constant",
      "-Xlint:delayedinit-select",
      "-Xlint:doc-detached",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:nullary-override",
      "-Xlint:nullary-unit",
      "-Xlint:option-implicit",
      "-Xlint:package-object-classes",
      "-Xlint:poly-implicit-overload",
      "-Xlint:private-shadow",
      "-Xlint:stars-align",
      "-Xlint:type-parameter-shadow",
      "-Xlint:unsound-match"
    ),
    Compile / console / scalacOptions := Seq(
      // standard settings
      "-target:jvm-1.8",
      "-encoding",
      "UTF-8",
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      // language features
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      // private options
      "-Yno-adapted-args",
      "-Ypartial-unification"
    ),
    Test / console / scalacOptions := Seq(
      // standard settings
      "-target:jvm-1.8",
      "-encoding",
      "UTF-8",
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      // language features
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      // private options
      "-Yno-adapted-args",
      "-Ypartial-unification"
    ),
    Global / cancelable := true,
    Compile / fork := true,
    Compile / trapExit := false,
    Compile / connectInput := true,
    Compile / outputStrategy := Some(StdoutOutput),
    resolvers ++= commonResolvers,
    libraryDependencies ++= mainDeps,
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4"),
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary),
    Compile / scalafmtOnCompile := true,
    scalastyleFailOnError := true,
    Compile / compile / wartremoverWarnings ++= Warts.allBut(
      Wart.Any,
      Wart.DefaultArguments,
      Wart.ExplicitImplicitTypes,
      Wart.ImplicitConversion,
      Wart.ImplicitParameter,
      Wart.Overloading,
      Wart.PublicInference,
      Wart.NonUnitStatements,
      Wart.Nothing
    )
  )

  implicit final class RunConfigurator(project: Project) {

    def configureRun(main: String): Project =
      project
        .settings(
          inTask(assembly)(
            Seq(
              assemblyJarName := s"${name.value}.jar",
              assemblyMergeStrategy := {
                case strategy => MergeStrategy.defaultMergeStrategy(strategy)
              },
              mainClass := Some(main)
            )
          )
        )
        .settings(Compile / run / mainClass := Some(main))
  }

  sealed abstract class TestConfigurator(project: Project, config: Configuration) {

    protected def configure(requiresFork: Boolean): Project =
      project
        .configs(config)
        .settings(inConfig(config)(Defaults.testSettings): _*)
        .settings(inConfig(config)(scalafmtSettings))
        .settings(
          inConfig(config)(
            Seq(
              scalafmtOnCompile := true,
              scalastyleConfig := baseDirectory.value / "scalastyle-test-config.xml",
              scalastyleFailOnError := false,
              fork := requiresFork,
              testFrameworks := Seq(Specs2)
            )
          )
        )
        .settings(libraryDependencies ++= testDeps map (_ % config.name))
        .enablePlugins(ScoverageSbtPlugin)

    protected def configureSequential(requiresFork: Boolean): Project =
      configure(requiresFork).settings(
        inConfig(config)(
          Seq(
            testOptions += Argument(Specs2, "sequential"),
            parallelExecution := false
          )
        )
      )
  }

  implicit final class DataConfigurator(project: Project) {

    def setName(newName: String): Project = project.settings(name := newName)

    def setDescription(newDescription: String): Project = project.settings(description := newDescription)

    def setInitialImport(newInitialCommand: String*): Project =
      project.settings(initialCommands := s"import ${("dddblueprint._" +: newInitialCommand.toList).mkString(", ")}")
  }

  implicit final class RootConfigurator(project: Project) {

    def configureRoot: Project = project.settings(rootSettings: _*)
  }

  implicit final class ModuleConfigurator(project: Project) {

    def configureModule: Project = project.settings(modulesSettings: _*).enablePlugins(GitVersioning)
  }

  implicit final class UnitTestConfigurator(project: Project) extends TestConfigurator(project, Test) {

    def configureTests(requiresFork: Boolean = false): Project = configure(requiresFork)

    def configureTestsSequential(requiresFork: Boolean = false): Project = configureSequential(requiresFork)
  }

  implicit final class FunctionalTestConfigurator(project: Project) extends TestConfigurator(project, FunctionalTest) {

    def configureFunctionalTests(requiresFork: Boolean = false): Project = configure(requiresFork)

    def configureFunctionalTestsSequential(requiresFork: Boolean = false): Project = configureSequential(requiresFork)
  }

  implicit final class IntegrationTestConfigurator(project: Project)
      extends TestConfigurator(project, IntegrationTest) {

    def configureIntegrationTests(requiresFork: Boolean = false): Project = configure(requiresFork)

    def configureIntegrationTestsSequential(requiresFork: Boolean = false): Project = configureSequential(requiresFork)
  }
}
