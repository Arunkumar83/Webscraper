name := "WebScraper"

version := "0.1"

//scalaVersion := "2.12.8"

scalaVersion := "2.12.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions += "-Ypartial-unification" // 2.11.9+

libraryDependencies += "org.jsoup" % "jsoup" % "1.12.1"