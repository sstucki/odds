import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ch.epfl.lamp",
    version := "0.1-SNAPSHOT",
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := "2.10.2-RC1",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= Seq(
      "-deprecation", "-unchecked", "-feature", "-Xexperimental",
      "-Yvirtualize", "-language:higherKinds"),
    //scalacOptions += "-Xlog-implicits",
    //scalacOptions += "-Ymacro-debug-lite",
    autoAPIMappings := true,
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    //libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT",
    addCompilerPlugin("org.scala-lang.virtualized.plugins" % "macro-paradise_2.10.2-RC1" % "2.0.0-SNAPSHOT"))
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ Seq(
      name := "odds",
      run <<= run in Compile in core
    )
  ) aggregate(macros, core, common)

  lazy val common = Project(
    id = "common",
    base = file("common"),
    settings = buildSettings
  )

  lazy val macros = Project(
    id = "macros",
    base = file("macros"),
    settings = buildSettings ++ Seq(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
  ) dependsOn(common)

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildSettings
  ) dependsOn(macros, common)
}
