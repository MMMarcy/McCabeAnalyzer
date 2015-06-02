name := "McCabeAnalyzer"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= List(
  "com.h2database" % "h2" % "1.4.187",
  "com.typesafe.slick" %% "slick" % "3.0.0",
  "com.typesafe" % "config" % "1.3.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.googlecode.juniversalchardet" % "juniversalchardet" % "1.0.3"
)
    