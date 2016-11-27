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

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.util.Timeout
import org.scalatest._
import org.scalatest.concurrent.{ Eventually, ScalaFutures }
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.inject.BindingKey
import play.api.libs.json.{ JsValue, Json, Writes }
import play.api.test.WsTestClient

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.reflect.ClassTag
import scala.util.Try

object Person {
  implicit val format = Json.format[Person]
  implicit class ValueObjectOps(val self: Person) {
    def toJson: JsValue = Json.toJson(self)
  }
  implicit class IterableOps(val self: Iterable[Person]) {
    def toJson: JsValue = Json.toJson(self)
  }
}
final case class Person(firstName: String, age: Int)

class TestSpec extends FlatSpec
    with Matchers
    with GivenWhenThen
    with OptionValues
    with TryValues
    with ScalaFutures
    with WsTestClient
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with GuiceOneServerPerSuite {

  def getComponent[A: ClassTag] = app.injector.instanceOf[A]
  def getNamedComponent[A](name: String)(implicit ct: ClassTag[A]): A =
    app.injector.instanceOf[A](BindingKey(ct.runtimeClass.asInstanceOf[Class[A]]).qualifiedWith(name))

  // set the port number of the HTTP server
  override lazy val port: Int = getNamedComponent[Int]("test.port")
  implicit val timeout: Timeout = getComponent[Timeout]
  implicit val pc: PatienceConfig = PatienceConfig(timeout = 30.seconds, interval = 300.millis)
  implicit val system: ActorSystem = getComponent[ActorSystem]
  implicit val ec: ExecutionContext = getComponent[ExecutionContext]
  implicit val mat: Materializer = getComponent[Materializer]

  // ================================== Supporting Operations ====================================
  def id: String = java.util.UUID.randomUUID().toString

  implicit class PimpedFuture[T](self: Future[T]) {
    def toTry: Try[T] = Try(self.futureValue)
  }

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

  override protected def beforeEach(): Unit = {
  }
}
