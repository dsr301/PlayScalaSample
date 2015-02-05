import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "nlApp"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.21",
    "securesocial" %% "securesocial" % "master-SNAPSHOT",
    "com.google.guava" % "guava" % "14.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers += Resolver.url("sbt-plugin-snapshots", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns)
  )

}
