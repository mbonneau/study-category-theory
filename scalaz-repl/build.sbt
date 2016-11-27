scalaVersion := "2.12.0"

val scalazVersion = "7.2.7"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
    "org.typelevel" %% "scalaz-outlaws" % "0.2"
)

initialize ~= { _ =>
  val ansi = System.getProperty("sbt.log.noformat", "false") != "true"
  if (ansi) System.setProperty("scala.color", "true")
}

initialCommands in console := """
import scalaz._, Scalaz._
import scalaz.syntax.foldable1._ //  for foldable structures that are guaranteed to have at least one element
import scalaz.concurrent._ 
import scala.concurrent._
import scala.collection.immutable._
import scalaz.outlaws.std.utilTry._
import scala.concurrent.ExecutionContext.Implicits.global
"""
