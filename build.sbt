name := "study-category-theory"

organization := "com.github.dnvriend"

version := "1.0.0"

scalaVersion := "2.11.7"

libraryDependencies ++= {
  val akkaVersion = "2.4.1"
  val akkaStreamAndHttpVersion = "2.0-M2"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream-experimental" % akkaStreamAndHttpVersion,
    "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaStreamAndHttpVersion,
    "org.scalaz" %% "scalaz-core" % "7.2.0",
    "org.spire-math" %% "cats" % "0.3.0",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % Test,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
    "org.scalatest" %% "scalatest" % "2.2.4" % Test,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % Test
  )
}

fork in Test := true

parallelExecution := false

licenses +=("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

// enable scala code formatting //
import scalariform.formatter.preferences._

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 100)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)

// enable updating file headers //
import de.heikoseeberger.sbtheader.license.Apache2_0

headers := Map(
  "scala" -> Apache2_0("2015", "Dennis Vriend"),
  "conf" -> Apache2_0("2015", "Dennis Vriend", "#")
)

enablePlugins(AutomateHeaderPlugin)