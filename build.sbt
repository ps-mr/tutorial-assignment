scalaVersion := "2.11.7"

scalaSource in Compile := baseDirectory.value / "main"

scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
