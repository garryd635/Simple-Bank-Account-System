name := "bank"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.6"

connectInput in run := true

libraryDependencies ++= Seq(
  "org.jsoup" %  "jsoup" % "1.10.1"
)


