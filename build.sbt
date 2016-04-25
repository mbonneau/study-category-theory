name := "study-category-theory"

organization := "com.github.dnvriend"

version := "1.0.0"

scalaVersion := "2.11.8"

// http://scala-hamsters.github.io/hamsters/releases/io.github.scala-hamsters/hamsters_2.11/1.0.0-BETA1/ivys/ivy.xml
resolvers += Resolver.url("github repo for hamsters", url("http://scala-hamsters.github.io/hamsters/releases/"))(Resolver.ivyStylePatterns)

libraryDependencies ++= {
  val akkaVersion = "2.4.3"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
    "io.github.scala-hamsters" %% "hamsters" % "1.0.0-BETA1",
    "org.scalaz" %% "scalaz-core" % "7.2.2",
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
    "org.typelevel" %% "scalaz-scalatest" % "0.3.0" % Test,
    "org.scalatest" %% "scalatest" % "2.2.5" % Test
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
  "scala" -> Apache2_0("2016", "Dennis Vriend"),
  "conf" -> Apache2_0("2016", "Dennis Vriend", "#")
)

enablePlugins(AutomateHeaderPlugin)