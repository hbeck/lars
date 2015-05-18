name := "lars"

version := "1.0"

scalaVersion := "2.11.6" //2.11.6 incompatibility with sbt; 2.10.4

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions += "-feature"