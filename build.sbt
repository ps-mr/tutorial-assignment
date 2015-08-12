scalaVersion := "2.11.7"

scalaSource in Compile := baseDirectory.value / "main"

scalaSource in Test := baseDirectory.value / "test"
