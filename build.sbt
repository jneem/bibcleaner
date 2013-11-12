name := "bibcleaner"

version := "0.1"

scalaVersion := "2.10.3"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil


// JUnit

libraryDependencies += "junit" % "junit" % "4.11" % "test"

// ScalaCheck

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.0" % "test"


// Scalatest

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"


// Parboiled

libraryDependencies ++= Seq(
  "org.parboiled" % "parboiled-core" % "1.1.5",
  "org.parboiled" %% "parboiled-scala" % "1.1.5"
)


// Slick

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "1.0.1",
  "org.slf4j" % "slf4j-nop" % "1.6.4"
)


// Play JSON

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.1"

