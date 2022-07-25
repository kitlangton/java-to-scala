ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version      := "0.1.0-SNAPSHOT"

val zioVersion = "2.0.0"

addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")

lazy val root = (project in file("."))
  .settings(
    name := "java-to-scala",
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "pprint"       % "0.7.3",
      "dev.zio"       %% "zio"          % zioVersion,
      "org.typelevel" %% "cats-effect"  % "3.3.11",
      "com.twitter"   %% "finagle-http" % "22.4.0",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-encoding",
      "UTF-8",
      "-Xlint",
      "-Xverify",
//      "-Xfatal-warnings",
      "-feature",
      "-language:_",
      "-Wconf:cat=unused:info"
    )
  )
