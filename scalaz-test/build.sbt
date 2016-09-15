name := "scalaz-test"

version := "1.0.0"

scalaVersion := "2.11.8"

libraryDependencies ++= {
  val scalazVersion = "7.2.6"
  val scalazContribVersion = "0.2"
  Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
//  "org.typelevel" %% "scalaz-contrib-210" % scalazContribVersion,
//  "org.typelevel" %% "scalaz-contrib-validation" % scalazContribVersion,
//  "org.typelevel" %% "scalaz-contrib-undo" % scalazContribVersion,
  // currently unavailable because there's no 2.11 build of Lift yet
  // "org.typelevel" %% "scalaz-lift"               % "0.2",
//  "org.typelevel" %% "scalaz-nscala-time" % scalazContribVersion,
//  "org.typelevel" %% "scalaz-spire" % scalazContribVersion,
  "org.typelevel" %% "scalaz-scalatest" % "1.0.0" % Test,
  "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )
}

testFrameworks += new TestFramework("utest.runner.Framework")

licenses += ("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

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