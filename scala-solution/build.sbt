scalaVersion := "2.13.8"

name := "scala-solution"
organization := "com.angelcerveraroldan"
version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scalactic" %% "scalactic" % "3.2.13" % Test,
  "org.scalatest" %% "scalatest" % "3.2.13" % Test
)
