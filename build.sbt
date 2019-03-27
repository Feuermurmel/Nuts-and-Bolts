scalaVersion := "2.12.8"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-Xfatal-warnings"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.7.0"

val nuts_and_bolts = project.in(file(""))
