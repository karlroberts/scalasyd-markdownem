enablePlugins(ScalaJSPlugin)

// This is an application with a main method
// change this to true if you want the The TestApp main class to be a JS "Application"
scalaJSUseMainModuleInitializer := true

name := "Markdownem"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

mainClass in Compile := Some("tutorial.webapp.TestApp")

libraryDependencies ++= Seq("org.specs2" %%% "specs2-core" % "4.0.0-RC4" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")