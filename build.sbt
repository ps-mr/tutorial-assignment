scalaVersion := "2.11.7"

scalacOptions := List("-feature", "-deprecation")

scalaSource in Compile := baseDirectory.value / "main"

scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "1.1.5"

libraryDependencies += "com.github.vagm" %% "optimus" % "1.2.1"
