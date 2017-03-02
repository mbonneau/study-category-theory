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

package controllers

import javax.inject.Inject

import akka.pattern.CircuitBreaker
import play.api.mvc._
import play.api.libs.json._

import scala.concurrent.{ ExecutionContext, Future }

final case class Number(theAnswerToLifeTheUniverseAndEverything: Int)
object Number {
  implicit val format = Json.format[Number]
}

class ApplicationController @Inject() (cb: CircuitBreaker)(implicit ec: ExecutionContext) extends Controller {
  def hello = Action(Ok("Hello World!"))

  def theAnswerToLifeTheUniverseAndEverything = Action(Ok(Json.toJson(Number(constants.Constants.TheAnswerToLifeTheUniverseAndEverything))))

  def fail = Action.async(cb.withCircuitBreaker(Future.failed(new RuntimeException("This should fail")).map(_ => Ok("Hello World!"))))
}
