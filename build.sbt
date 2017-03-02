name := "study-category-theory"

organization := "com.github.dnvriend"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel" // use Typelevel's Scala instead of Lightbend Scala

resolvers += "scalaz" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val akkaVersion = "2.4.17"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "io.github.scala-hamsters" %% "hamsters" % "1.1.1"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.9"
libraryDependencies += "org.typelevel" %% "scalaz-outlaws" % "0.3"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test
libraryDependencies += "org.typelevel" %% "scalaz-scalatest" % "1.1.1" % Test
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0-M2" % Test

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

enablePlugins(AutomateHeaderPlugin, PlayScala)

lazy val catsTest = project in file("cats-test")

// =======================================
// ==== Lightbend Orchestration (ConductR)
// =======================================
// read: https://github.com/typesafehub/conductr-lib#play25-conductr-bundle-lib
// =======================================
enablePlugins(PlayBundlePlugin)

// Declares endpoints. The default is Map("web" -> Endpoint("http", 0, Set.empty)).
// The endpoint key is used to form a set of environment variables for your components,
// e.g. for the endpoint key "web" ConductR creates the environment variable WEB_BIND_PORT.
BundleKeys.endpoints := Map(
  "play" -> Endpoint(bindProtocol = "http", bindPort = 0, services = Set(URI("http://:9000/play"))),
  "akka-remote" -> Endpoint("tcp")
)

normalizedName in Bundle := name.value // the human readable name for your bundle

BundleKeys.system := name.value + "system" // represents the clustered ActorSystem

BundleKeys.startCommand += "-Dhttp.address=$PLAY_BIND_IP -Dhttp.port=$PLAY_BIND_PORT"

// ====================================
// ==== Lightbend Monitoring (Cinnamon)
// ====================================
// Enable the Cinnamon Lightbend Monitoring sbt plugin
enablePlugins (Cinnamon)

libraryDependencies += Cinnamon.library.cinnamonSandbox

// Add the Monitoring Agent for run and test
cinnamon in run := true
cinnamon in test := true