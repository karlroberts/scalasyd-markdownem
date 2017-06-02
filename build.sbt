// This is an application with a main method
enablePlugins(ScalaJSPlugin)

// change this to true if you want the T
scalaJSUseMainModuleInitializer := true

name := "Markdownem"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

mainClass in Compile := Some("tutorial.webapp.TestApp")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.9.5" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")