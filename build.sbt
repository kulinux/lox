lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.lox",
      scalaVersion := "3.5.2"
    )),
    name := "lox"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.typelevel" %% "cats-core" % "2.13.0"
)
