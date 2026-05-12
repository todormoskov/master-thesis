scalaVersion := "3.7.3"
version := "0.0.0"
name := "todormoskov"
organization := "com.todormoskov"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// We set the release target explicitly to Java 17,
// in order to ensure reproducible builds.
// (==> The resulting jar requires Java 17 or newer)
scalacOptions ++= Seq(
  "-release", "17"
)

assembly / mainClass := Some("com.todormoskov.Main")

assembly / assemblyJarName := s"epc2tla-${version.value}.jar"
