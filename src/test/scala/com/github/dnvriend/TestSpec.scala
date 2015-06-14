package com.github.dnvriend

import java.util.UUID

import akka.actor._
import akka.event.{Logging, LoggingAdapter}
import akka.stream.{FlowMaterializer, ActorFlowMaterializer}
import akka.util.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import spray.json.DefaultJsonProtocol

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class Person(name: String, age: Int)

trait TestSpec extends FlatSpec with Matchers with ScalaFutures with BeforeAndAfterAll with DefaultJsonProtocol {
  implicit val timeout: Timeout = Timeout(5.seconds)
  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val flowMaterializer: FlowMaterializer = ActorFlowMaterializer()
  implicit val log: LoggingAdapter = Logging(system, this.getClass)
  implicit val personJsonFormat = jsonFormat2(Person)
  implicit val pc: PatienceConfig = PatienceConfig(timeout = 50.seconds)

  type UUIDAsString = String
  def randomId: UUIDAsString = UUID.randomUUID.toString
  val id: UUIDAsString = randomId

  /**
   * Final vals get inlined in bytecode. It does the same as Java's 'final static' variant.
   * Leaving final from the val will not inline the constant (right hand side) in bytecode.
   * see: http://stackoverflow.com/questions/13412386/why-are-private-val-and-private-final-val-different
   *
   * To be a constant, the first letter needs to be uppercase:
   * see: http://www.artima.com/pins1ed/functional-objects.html#6.10
   */
  final val FirstName: String = "John"
  final val LastName: String = "Doe"

  implicit class FutureToTry[T](f: Future[T]) {
    def toTry: Try[T] = Try(f.futureValue)
  }

  override protected def afterAll(): Unit = {
    system.shutdown()
    system.awaitTermination()
  }
}