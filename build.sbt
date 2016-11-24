name := "study-category-theory"

organization := "com.github.dnvriend"

version := "1.0.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "scalaz" at "http://dl.bintray.com/scalaz/releases",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots"
)

libraryDependencies ++= {
  val akkaVersion = "2.4.11"
  val scalazVersion = "7.2.6"
  val hamstersVersion = "1.0.7"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
    "io.github.scala-hamsters" %% "hamsters" % hamstersVersion,
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.typelevel" %% "scalaz-outlaws" % "0.2",
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
    "org.typelevel" %% "scalaz-scalatest" % "1.1.0" % Test,
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )
}

scalacOptions ++= Seq("-feature", "-language:higherKinds", "-language:implicitConversions", "-deprecation", "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8", "-Xexperimental")

fork in Test := true

parallelExecution := false

licenses +=("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

// enable scala code formatting //
import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform

// Scalariform settings
SbtScalariform.autoImport.scalariformPreferences := SbtScalariform.autoImport.scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 100)
  .setPreference(DoubleIndentClassDeclaration, true)

// enable updating file headers //
import de.heikoseeberger.sbtheader.license.Apache2_0

headers := Map(
  "scala" -> Apache2_0("2016", "Dennis Vriend"),
  "conf" -> Apache2_0("2016", "Dennis Vriend", "#")
)

enablePlugins(AutomateHeaderPlugin)