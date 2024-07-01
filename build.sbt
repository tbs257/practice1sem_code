ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "practice1sem_code",
    fork := true,
    Compile / mainClass := Some("MainGUI"),
  )

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-core"   % "2.12.0",
  "org.typelevel"          %% "cats-effect" % "3.3.12",
  "org.scalanlp"           %% "breeze"      % "2.1.0",
  "org.scalanlp"           %% "breeze-viz"  % "2.1.0",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
)
