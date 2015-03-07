name := """oldtobyapi-acceptancetests"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.typesafe.play" %% "play-ws" % "2.3.8" % "test"
)

resolvers += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"