name := """astar"""

version := "1.0"

scalaVersion := "2.11.4"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"


scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"


// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

