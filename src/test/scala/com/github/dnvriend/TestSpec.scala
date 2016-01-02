/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend

import java.util.UUID

import akka.actor._
import akka.event.{ Logging, LoggingAdapter }
import akka.stream.{ ActorMaterializer, Materializer }
import akka.util.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ GivenWhenThen, BeforeAndAfterAll, FlatSpec, Matchers }
import spray.json.DefaultJsonProtocol

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

case class Person(name: String, age: Int)

trait TestSpec extends FlatSpec with GivenWhenThen with Matchers with ScalaFutures with BeforeAndAfterAll with DefaultJsonProtocol {
  implicit val timeout: Timeout = Timeout(5.seconds)
  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val materializer: Materializer = ActorMaterializer()
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
    system.terminate()
    system.whenTerminated.toTry should be a 'success
  }
}
