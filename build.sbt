name := "maxsmt-playground"

version := "1.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.1.0" % "test",
                            "org.scalaz" %% "scalaz-core" % "7.0.4")

(fork := true) ++ (javaOptions += "-Xss10M")
