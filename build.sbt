name := "study-category-theory"

version := "1.0.0"

scalaVersion := "2.11.6"

resolvers += "dnvriend at bintray" at "http://dl.bintray.com/dnvriend/maven"

libraryDependencies ++= {
  val akkaVersion  = "2.3.11"
  val streamVersion = "1.0-RC3"
  Seq(
    "com.typesafe.akka"  %%  "akka-actor"                       % akkaVersion,
    "com.typesafe.akka"  %%  "akka-slf4j"                       % akkaVersion,
    "com.typesafe.akka"  %%  "akka-stream-experimental"         % streamVersion,
    "com.typesafe.akka"  %%  "akka-http-core-experimental"      % streamVersion,
    "io.spray"           %%  "spray-json"                       % "1.3.2",
    "org.scalaz"         %%  "scalaz-core"                      % "7.1.2",
    "com.typesafe.akka"  %%  "akka-testkit"                     % akkaVersion % Test,
    "org.scalatest"      %%  "scalatest"                        % "2.2.4"     % Test,
    "org.scalamock"      %%  "scalamock-scalatest-support"      % "3.2"       % Test
  )
}

fork in Test := true

parallelExecution in Test := false