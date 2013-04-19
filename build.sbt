name := "odds"

organization := "EPFL"

version := "0.1-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.1-V1")

scalaBinaryVersion := Option(System.getenv("SCALA_VERSION")).getOrElse("2.10.1")

autoScalaLibrary := false

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots,
    "Sonatype Public" at "https://oss.sonatype.org/content/groups/public")

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test" intransitive(),
    "EPFL" % "lms_2.10.1" % "0.3-SNAPSHOT" intransitive(),
    "org.scala-lang.virtualized" % "scala-library" % "2.10.1-V1")

//--- End of Dependencies

// General compiler options
scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable",
  "-Yvirtualize", "-feature", "-language:higherKinds")

// Documentation (scaladoc) options
scalacOptions in doc += "-external-urls:scala=http://www.scala-lang.org/"

// Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % ver)
}
