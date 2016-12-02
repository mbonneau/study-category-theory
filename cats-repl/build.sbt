scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"
 
resolvers += "scalaz" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/stew/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

scalacOptions += "-Ypartial-unification" // enable fix for SI-2712
scalacOptions += "-Yliteral-types"       // enable SIP-23 implementation

initialize ~= { _ =>
  val ansi = System.getProperty("sbt.log.noformat", "false") != "true"
  if (ansi) System.setProperty("scala.color", "true")
}

initialCommands in console := """
import cats._
import cats.implicits._
import scala.concurrent._
import scala.collection.immutable._
import shapeless._
import scala.reflect.runtime.universe._
import scala.concurrent.ExecutionContext.Implicits.global
final case class Person(name: String, age: Int)
final case class Cat(name: String, age: Int)
val dennis = Person("Dennis", 42)
val elsa = Cat("Elsa", 18)
val tijger = Cat("Tijger", 13)
val guys = List(elsa, tijger)
"""
