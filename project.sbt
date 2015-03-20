name := "q-language"

description := "A project to implement the Q language for demostrating referential transparency"

scalaVersion := "2.11.4"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val ILDScalaPractice = FDProject("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    							      "org.scalatest" %% "scalatest" % "2.2.1" % "test")

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"