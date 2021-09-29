// Created using Giter8 Templates


name := "nga-scala"
version := "0.1.2"
scalaVersion := "3.0.0"

lazy val vm = project
	.in(file("nga-vm"))

lazy val assembler = project
	.in(file("nga-assembler"))
