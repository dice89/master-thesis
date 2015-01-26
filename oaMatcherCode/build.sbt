name := """ontology matching"""

version := "1.0"

scalaVersion := "2.10.4"

// Change this to another test framework if you prefer

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

// Uncomment to use Akka
libraryDependencies += "edu.cmu.lti" % "ws4j" % "1.0.1"  

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.1.1"

libraryDependencies += "net.debasishg" %% "redisclient" % "2.13"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.8"

libraryDependencies += "de.unima" % "alignmentapi.mavenized" % "4.6"

libraryDependencies += "secondstring" % "secondstring" % "20120620"

libraryDependencies += "net.sf.jwordnet" % "jwnl" % "1.4_rc3"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.2.0"

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "1.2.0"


resolvers += "Local Maven Repository" at "file:///Users/mueller/.m2/repository"

resolvers += "Third Party" at "http://trianacode.org/maven/"