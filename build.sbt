val zioVersion        = "2.0.13"
val zioJsonVersion    = "0.5.0"
val zioLoggingVersion = "2.1.12"

val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        organization := "net.degoes",
        scalaVersion := "2.13.10"
      )
    ),
    name           := "jvm-perf",
    javacOptions ++= Seq(
      "--add-modules",
      "jdk.incubator.vector",
      "--add-exports",
      "java.base/jdk.internal.vm.vector=ALL-UNNAMED"
    ),
    console / javaOptions ++= Seq(
      "--add-modules",
      "jdk.incubator.vector"
    ),
    // Needed to run the jdk.incubator.vector code.
    Jmh / javaOptions ++= Seq(
      "--add-modules",
      "jdk.incubator.vector"
    ),
    libraryDependencies ++= Seq(
      // general
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "dev.zio" %% "zio"      % zioVersion,

      // logging
      "dev.zio" %% "zio-logging"       % zioLoggingVersion,
      "dev.zio" %% "zio-logging-slf4j" % zioLoggingVersion,

      // test
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .enablePlugins(JmhPlugin)
