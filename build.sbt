ThisBuild / scalaVersion := "2.13.7"
ThisBuild / version      := "0.1.0-SNAPSHOT"

val zioVersion = "1.0.12"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    name := "java-to-scala",
    // add kind projector
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-encoding",
      "UTF-8",
      "-Xlint",
      "-Xverify",
      "-Xfatal-warnings",
      "-feature",
      "-language:_",
      "-Wconf:cat=unused:info"
    )
  )
