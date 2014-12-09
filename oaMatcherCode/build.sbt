name := """ontology matching"""

version := "1.0"

scalaVersion := "2.11.1"

// Change this to another test framework if you prefer

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

// Uncomment to use Akka

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.1.1"

libraryDependencies += "de.unima" % "alignmentapi.mavenized" % "4.6"

libraryDependencies += "secondstring" % "secondstring" % "20120620"

libraryDependencies += "net.sf.jwordnet" % "jwnl" % "1.4_rc3"

resolvers += "Local Maven Repository" at "file:///Users/mueller/.m2/repository"

resolvers += "Third Party" at "http://trianacode.org/maven/"