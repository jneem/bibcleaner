name := "bibcleaner"

version := "0.1"

scalaVersion := "2.11.2"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil


// JUnit

libraryDependencies += "junit" % "junit" % "4.11" % "test"

// ScalaCheck

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"


// Scalatest

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.4" % "test"


// Parboiled

libraryDependencies ++= Seq(
  "org.parboiled" % "parboiled-core" % "1.1.6",
  "org.parboiled" %% "parboiled-scala" % "1.1.6"
)


// Slick

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "2.1.0",
  "com.h2database" % "h2" % "1.4.179",
  "ch.qos.logback" % "logback-classic" % "1.1.2"
  //"org.slf4j" % "slf4j-nop" % "1.6.4"
)


// Play JSON

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.4"

