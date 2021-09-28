// Created using Giter8 Templates
name := "nga-scala"
version := "0.1.x"
scalaVersion := "3.0.0"


libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.2.9" % Test
	)

testOptions in Test += Tests.Argument("-oD")
