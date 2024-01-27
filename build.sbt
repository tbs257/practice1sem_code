ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "practice1sem_code",
    fork := true,
  )

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.9.0",
  "org.typelevel" %% "cats-effect" % "3.4.8",
  "org.scalanlp" %% "breeze" % "2.1.0",
  "org.scalanlp" %% "breeze-viz" % "2.1.0",
)
