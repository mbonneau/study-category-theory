name := "study-category-theory"

organization := "com.github.dnvriend"

version := "1.0.0"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel" // use Typelevel's Scala instead of Lightbend Scala

resolvers += "scalaz" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val akkaVersion = "2.4.14"

//libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "io.github.scala-hamsters" %% "hamsters" % "1.1.1"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7"
libraryDependencies += "org.typelevel" %% "scalaz-outlaws" % "0.3"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test
libraryDependencies += "org.typelevel" %% "scalaz-scalatest" % "1.1.1" % Test
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0-M1" % Test

scalacOptions += "-Ypartial-unification" // enable fix for SI-2712
scalacOptions += "-Yliteral-types"       // enable SIP-23 implementation
scalacOptions += "-language:higherKinds"
scalacOptions += "-Ybackend:GenBCode"
scalacOptions += "-Ydelambdafy:method"
scalacOptions += "-language:implicitConversions"
scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-Xexperimental"
scalacOptions += "-target:jvm-1.8"

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

lazy val catsTest =
  (project in file("cats-test"))