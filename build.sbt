organization  := "org.ciroque"

version       := "0.1"

scalaVersion  := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  Seq(
    "org.scalatest"       % "scalatest_2.11"  % "2.2.1"   % "test",
    "org.specs2" %% "specs2-core" % "2.4.11" % "test"
  )
}
