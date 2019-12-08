name := "advent-of-code-2019"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.6.1" % Test
)

testOptions in Test += Tests.Argument("-oD")

