lazy val commonSettings = Seq(
  organization := "lt.vpranckaitis",
  name := "yamlg",
  version := "0.1.0",
  scalaVersion := "2.11.5"
)

lazy val repositories =  Seq(
	resolvers += "org.sedis" at "http://pk11-scratch.googlecode.com/svn/trunk"
)

lazy val dependencies = Seq(
  libraryDependencies += "redis.clients" % "jedis" % "2.6.2",
  
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  
  libraryDependencies += "io.spray" %% "spray-routing" % "1.3.1",
  libraryDependencies += "io.spray" %% "spray-can" % "1.3.1"
)

lazy val dependenciesTest = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test",
  libraryDependencies += "org.sedis" %% "sedis" % "1.2.2" % "test"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(repositories: _*).
  settings(dependencies: _*).
  settings(dependenciesTest: _*)