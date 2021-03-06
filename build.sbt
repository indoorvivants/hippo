val V = new {
  val Scala = "3.1.0"

  val laminar         = "0.13.1"
  val http4s          = "0.23.6"
  val sttp            = "3.3.15"
  val circe           = "0.14.1"
  val decline         = "2.2.0"
  val organiseImports = "0.5.0"
  val weaver          = "0.7.7"
  val scodec          = "2.0.0"
  val scodecBits      = "1.1.29"
  val waypoint        = "0.4.2"
}

val Dependencies = new {
  private val http4sModules =
    Seq("dsl", "blaze-server", "ember-client", "ember-server", "circe").map(
      "http4s-" + _
    )

  private val sttpModules = Seq("core", "circe")

  lazy val frontend = Seq(
    libraryDependencies ++=
      Seq(
        "com.softwaremill.sttp.client3" %%% "core"         % V.sttp,
        "com.softwaremill.sttp.client3" %%% "circe"        % V.sttp,
        "io.circe"                      %%% "circe-parser" % V.circe,
        "com.raquo"                     %%% "laminar"      % V.laminar,
        "com.raquo"                     %%% "waypoint"     % V.waypoint
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
    libraryDependencies += "io.circe"   %%% "circe-core"   % V.circe,
    libraryDependencies += "io.circe"   %%% "circe-parser" % V.circe % Test,
    libraryDependencies += "org.scodec" %%% "scodec-bits"  % V.scodecBits
  )

  lazy val tests = Def.settings(
    libraryDependencies += "com.disneystreaming" %%% "weaver-cats" % V.weaver % Test,
    libraryDependencies += "com.disneystreaming" %%% "weaver-scalacheck" % V.weaver % Test,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
}

lazy val root =
  (project in file(".")).aggregate(
    frontend,
    backend,
    shared.js,
    shared.jvm,
    analyser.jvm,
    cli
  )

lazy val frontend = project
  .in(file("modules/frontend"))
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

lazy val cli = project
  .in(file("modules/cli"))
  .dependsOn(backend)
  .settings(commonBuildSettings)
  .settings(
    Compile / resourceGenerators +=
      Def.taskIf {
        if (sys.env.contains("CI")) {
          val out = (Compile / resourceManaged).value / "frontend.js"

          val fullOpt = (frontend / Compile / fullOptJS).value.data

          IO.copyFile(fullOpt, out)

          List(out)
        } else {
          val out = (Compile / resourceManaged).value / "frontend.js"

          val fastOpt = (frontend / Compile / fastOptJS).value.data

          IO.copyFile(fastOpt, out)

          List(out)

        }
      }.taskValue
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
  .settings(Dependencies.tests)

lazy val analyser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(shared)
  .in(file("modules/analyser"))
  .jvmSettings(Dependencies.analyser)
  .jsSettings(Dependencies.analyser)
  .jsSettings(commonBuildSettings)
  .jvmSettings(commonBuildSettings)

lazy val commonBuildSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := V.Scala
)

val CICommands = Seq(
  "scalafmtCheckAll",
  "clean",
  "compile",
  "test"
).mkString(";")

val PrepareCICommands = Seq(
  "Test/scalafmtAll",
  "scalafmtAll",
  "scalafmtSbt"
).mkString(";")

addCommandAlias("ci", CICommands)

addCommandAlias("fix", PrepareCICommands)
