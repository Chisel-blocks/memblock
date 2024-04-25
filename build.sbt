import scala.sys.process._
// OBS: sbt._ has also process. Importing scala.sys.process
// and explicitly using it ensures the correct operation

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := scala.sys.process.Process("git rev-parse --short HEAD").!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"
ThisBuild / organization     := "Chisel-blocks"

// Suppresses eviction errors for new sbt versions
ThisBuild / evictionErrorLevel := Level.Info

val chiselVersion = "3.5.1"

resolvers += "A-Core Gitlab" at "https://gitlab.com/api/v4/groups/13348068/-/packages/maven"

lazy val memblock = (project in file("."))
  .settings(
    name := "memblock",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "dsptools" % "1.5.6",
      "net.jcazevedo" %% "moultingyaml" % "0.4.2"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
  )

// Parse the version of a submodle from the git submodule status
// for those modules not version controlled by Maven or equivalent
def gitSubmoduleHashSnapshotVersion(submod: String): String = {
    val shellcommand =  "git submodule status | grep %s | awk '{print substr($1,0,7)}'".format(submod)
    scala.sys.process.Process(Seq("/bin/sh", "-c", shellcommand )).!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"
}

