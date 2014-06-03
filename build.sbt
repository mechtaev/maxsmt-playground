name := "maxsmt-playground"

version := "1.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.0.M6" % "test",
                            "org.scalaz" %% "scalaz-core" % "7.0.4")

(fork := true) ++ (javaOptions += "-Xss10M")
