name := """ontology matching"""

version := "1.0"

scalaVersion := "2.11.1"

// Change this to another test framework if you prefer

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

// Uncomment to use Akka
libraryDependencies += "org.apache.commons" % "commons-csv" % "1.0"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
