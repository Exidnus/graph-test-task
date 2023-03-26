ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "graph-test-task",
    libraryDependencies := Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.scalatest"               %% "scalatest"       % "3.2.15"   % "test"
    )
  )
