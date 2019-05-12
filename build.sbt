scalaVersion in ThisBuild := "2.12.8"
scalacOptions in ThisBuild += "-unchecked"
scalacOptions in ThisBuild += "-deprecation"
scalacOptions in ThisBuild += "-feature"
scalacOptions in ThisBuild += "-Xfatal-warnings"

val nuts_and_bolts = project
  .in(file(""))
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.7" % Test)
