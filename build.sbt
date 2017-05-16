import sbt._
import Keys._
//import com.owtelse.sbt.S3WebsitePlugin
//import com.owtelse.sbt.S3WebsitePlugin.S3WS._
import org.scalajs.sbtplugin.ScalaJSPlugin


  val mySettings = Defaults.coreDefaultSettings ++ Seq(
    name := "markdownem",
    version := "1.1",
    scalaVersion := "2.12.2",
    organization := "io.kirko",
    scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
    scalacOptions in Test ++= Seq("-Yrangepos")
    //    s3wsBucket := "docs.aws.kirko.com.au",
    //    s3wsAssetDir := baseDirectory.value / "docs_site/",
    //    progressBar in s3wsUpload := true,
    //    credentials += Credentials(Path.userHome / ".s3AvocadoCreds"),
    //    s3wsIncremental := true,
    //    s3wsPrefix := "mardownem-docs/"
  )

  //  val scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
  //  scalacOptions += "-target:jvm-1.8"


    val libs = Seq(
          "org.specs2"           %%   "specs2-core"       % "3.8.9" % "test",
          "org.specs2"           %%   "specs2-junit"      % "3.8.9" % "test",
          "org.specs2"           %%   "specs2-mock"       % "3.8.9" % "test",
          "org.scalacheck"       %%   "scalacheck"        % "1.13.5" % "test"  // withSources() withJavadoc(),
      //    "ch.qos.logback"          % "logback-classic"  % logbackV,
    )


  // Library dependencies
  lazy val root = Project("markdownem", file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings(mySettings: _*)
    .settings(libraryDependencies ++= libs)
  //  .settings(resolvers ++= repos)
  //  .settings(Seq(genDocsTask): _*)



