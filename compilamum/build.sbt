name := "compilamum"
version := "1.0"
scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                % "3.0.4" % "test",
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:postfixOps"
)
