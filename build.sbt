name := "netflowV9 Parser"

version := "1.0"

scalaVersion := "2.11.12"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

//libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.2"

// for debugging sbt problems
logLevel := Level.Debug

scalacOptions += "-deprecation"
