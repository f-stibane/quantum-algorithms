name := "qa2"
version := "0.1"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalacheck" % "scalacheck_2.12" % "1.13.4" % Test
)
