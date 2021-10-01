// Created using Giter8 Templates


ThisBuild / version := "0.1.2"
ThisBuild / scalaVersion := "3.0.0"

lazy val root = project
	.in(file("."))
	.aggregate(vm, assembler) 
	.dependsOn(vm, assembler)

lazy val vm = project
	.in(file("nga-vm"))

lazy val assembler = project
	.in(file("nga-assembler"))
	.dependsOn(vm)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
