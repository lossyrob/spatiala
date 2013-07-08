import sbt._
import sbt.Keys._

object Spatiala extends Build {

  val key = AttributeKey[Boolean]("javaOptionsPatched")

  lazy val index:Project = 
    Project("root", file(".")).settings(
      organization := "com.azavea.spatiala",
      name := "Spatiala",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "2.10.2",
      scalacOptions ++= Seq("-deprecation",
        "-unchecked",
        "-Yclosure-elim",
        "-Yinline-warnings",
        "-optimize",
        "-language:implicitConversions",
        "-language:postfixOps",
        "-language:existentials",
        "-feature"),
      scalacOptions in Compile in doc ++= Seq("-diagrams", "-implicits"),
      parallelExecution := false,

      fork in test := false,

      mainClass := Some("spatiala.Main"),

      libraryDependencies ++= Seq(
        "org.spire-math" %% "spire" % "0.3.0",
        "org.scalaz" %% "scalaz-core" % "7.0.0",
        "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
        "com.vividsolutions" % "jts" % "1.13",
        "net.sourceforge.jsi" % "jsi" % "1.0.0"
      ),
      resolvers ++= List(
        "Sourceforge JSI Repository" at "http://sourceforge.net/projects/jsi/files/m2_repo"
      ),
      licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.html")),
      homepage := Some(url("http://github.com/azavea/spatiala")),

      pomExtra := (
        <scm>
          <url>git@github.com:azavea/commonspace.git</url>
          <connection>scm:git:git@github.com:azavea/commonspace.git</connection>
          </scm>
          <developers>
          <developer>
          <id>lossyrob</id>
          <name>Rob Emanuele</name>
          <url>http://github.com/lossyrob/</url>
            </developer>
          </developers>
      )
    )
}
