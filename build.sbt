val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "goosea",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      "org.scodec" %% "scodec-core" % "2.1.0",
      "org.scodec" %% "scodec-bits" % "1.1.30",
    )
  )
