name := "sbt-mavgen"

organization := "net.creativepath"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.4"  // a scalate dependency doesn't like 2.11

sbtPlugin := true

libraryDependencies ++= Seq(
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
  "org.scalatest"        %% "scalatest"    % "2.2.1" % "test"
)
