name := "nga-vm"
version := "0.1.2"
scalaVersion := "3.0.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

