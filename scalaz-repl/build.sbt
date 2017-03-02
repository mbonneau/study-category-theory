scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"
 
resolvers += "scalaz" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.9"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.9"
libraryDependencies += "org.typelevel" %% "scalaz-outlaws" % "0.3"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

scalacOptions += "-Ypartial-unification" // enable fix for SI-2712
scalacOptions += "-Yliteral-types"       // enable SIP-23 implementation

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
import scala.reflect.runtime.universe._
import scala.concurrent.ExecutionContext.Implicits.global
import FutureOps._
final case class Person(name: String, age: Int)
final case class Cat(name: String, age: Int)
val dennis = Person("Dennis", 42)
val elsa = Cat("Elsa", 18)
val tijger = Cat("Tijger", 13)
val guys = List(elsa, tijger)
"""
