scalaVersion := "2.11.8"

val scalazVersion = "7.2.7"

resolvers ++= Seq(
  "scalaz" at "http://dl.bintray.com/scalaz/releases",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
    "org.typelevel" %% "scalaz-outlaws" % "0.2",
    "com.chuusai" %% "shapeless" % "2.3.2"
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
import shapeless._
import scala.concurrent.ExecutionContext.Implicits.global
final case class Person(name: String, age: Int)
final case class Cat(name: String, age: Int)
val dennis = Person("Dennis", 42)
val elsa = Cat("Elsa", 18)
val tijger = Cat("Tijger", 13)
val guys = List(elsa, tijger)
"""
