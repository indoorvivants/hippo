val V = new {
  val Scala = "3.1.0-RC2"

  val laminar         = "0.13.0"
  val http4s          = "0.23.2"
  val sttp            = "3.3.13"
  val circe           = "0.14.1"
  val decline         = "2.1.0"
  val organiseImports = "0.5.0"
  val weaver          = "0.7.6"
  val scodec          = "2.0.0"
  val scodecBits      = "1.1.28"
  val waypoint        = "0.4.2"
}

scalaVersion := V.Scala

val Dependencies = new {
  private val http4sModules =
    Seq("dsl", "blaze-server", "ember-client", "ember-server", "circe").map(
      "http4s-" + _
    )

  private val sttpModules = Seq("core", "circe")

  lazy val frontend = Seq(
    libraryDependencies ++=
      Seq(
        "com.softwaremill.sttp.client3" %%% "core"     % V.sttp,
        "com.softwaremill.sttp.client3" %%% "circe"    % V.sttp,
        "com.raquo"                     %%% "laminar"  % V.laminar,
        "com.raquo"                     %%% "waypoint" % V.waypoint
      )
  )

  lazy val backend = Seq(
    libraryDependencies ++=
      http4sModules.map("org.http4s" %% _         % V.http4s) ++
        Seq("com.monovore"           %% "decline" % V.decline)
  )

  lazy val analyser = Seq(
    libraryDependencies += "org.scodec" %% "scodec-core" % V.scodec
  )

  lazy val shared = Def.settings(
    libraryDependencies += "io.circe"   %%% "circe-core"  % V.circe,
    libraryDependencies += "org.scodec" %%% "scodec-bits" % V.scodecBits
  )

  lazy val tests = Def.settings(
    libraryDependencies += "com.disneystreaming" %%% "weaver-cats" % V.weaver % Test,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
}

lazy val root =
  (project in file(".")).aggregate(
    frontend,
    backend,
    shared.js,
    shared.jvm,
    analyser.jvm
  )

lazy val frontend = (project in file("modules/frontend"))
  .dependsOn(shared.js)
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(
    Dependencies.frontend,
    Dependencies.tests,
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .settings(commonBuildSettings)

lazy val backend = (project in file("modules/backend"))
  .dependsOn(shared.jvm, analyser.jvm)
  .settings(Dependencies.backend)
  .settings(Dependencies.tests)
  .settings(commonBuildSettings)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(
    Test / fork          := true,
    Universal / mappings += {
      val appJs = (frontend / Compile / fullOptJS).value.data
      appJs -> ("lib/prod.js")
    },
    Universal / javaOptions ++= Seq(
      "--port 8080",
      "--mode prod"
    ),
    Docker / packageName := "laminar-http4s-example"
  )

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/shared"))
  .jvmSettings(Dependencies.shared)
  .jsSettings(Dependencies.shared)
  .jsSettings(commonBuildSettings)
  .jvmSettings(commonBuildSettings)
  .settings(
    scalacOptions ++= Seq("-Xmax-inlines", "100")
  )

lazy val analyser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(shared)
  .in(file("modules/analyser"))
  .jvmSettings(Dependencies.analyser)
  .jsSettings(Dependencies.analyser)
  .jsSettings(commonBuildSettings)
  .jvmSettings(commonBuildSettings)

lazy val fastOptCompileCopy = taskKey[Unit]("")

val jsPath = "modules/backend/src/main/resources"

fastOptCompileCopy := {
  val source = (frontend / Compile / fastOptJS).value.data
  IO.copyFile(
    source,
    baseDirectory.value / jsPath / "dev.js"
  )
}

lazy val fullOptCompileCopy = taskKey[Unit]("")

fullOptCompileCopy := {
  val source = (frontend / Compile / fullOptJS).value.data
  IO.copyFile(
    source,
    baseDirectory.value / jsPath / "prod.js"
  )

}

lazy val commonBuildSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := V.Scala
)

addCommandAlias("runDev", ";fastOptCompileCopy; backend/reStart --mode dev")
addCommandAlias("runProd", ";fullOptCompileCopy; backend/reStart --mode prod")

val scalafixRules = Seq(
  "OrganizeImports",
  "DisableSyntax",
  "LeakingImplicitClassVal",
  "ProcedureSyntax",
  "NoValInForComprehension"
).mkString(" ")

val CICommands = Seq(
  "clean",
  "backend/compile",
  "backend/test",
  "frontend/compile",
  "frontend/fastOptJS",
  "frontend/test",
  "scalafmtCheckAll",
  s"scalafix --check $scalafixRules"
).mkString(";")

val PrepareCICommands = Seq(
  "test:scalafmtAll",
  "compile:scalafmtAll",
  "scalafmtSbt"
).mkString(";")

addCommandAlias("ci", CICommands)

addCommandAlias("preCI", PrepareCICommands)
