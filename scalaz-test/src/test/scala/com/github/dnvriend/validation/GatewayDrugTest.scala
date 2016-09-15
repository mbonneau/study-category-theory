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

package com.github.dnvriend.validation

import com.github.dnvriend.TestSpec

import scala.concurrent.Future
import scalaz.Scalaz._
import scalaz._

class GatewayDrugTest extends TestSpec {

  // see: http://eed3si9n.com/learning-scalaz/Validation.html

  final case class Foo(a: Int, b: Char, c: String)

  type ErrorsOr[A] = ValidationNel[String, A]
  type Validator[A] = String ⇒ ErrorsOr[A]

  val checkA: Validator[Int] = (s: String) ⇒
    try s.toInt.success catch {
      case _: NumberFormatException ⇒ "Not a number!".failureNel
    }

  val checkB: Validator[Char] = (s: String) ⇒
    if (s.length != 1 || s.head < 'a' || s.head > 'z') {
      "Not a lower case letter!".failureNel
    } else s.head.success

  val checkC: Validator[String] = (s: String) ⇒
    if (s.length == 4) s.success else "Wrong size!".failureNel

  // we can write a method that will lift our constructor into the ValidationNel applicative functor and apply it to our input
  def validateFoo(a: String, b: String, c: String) =
    (checkA(a) |@| checkB(b) |@| checkC(c))(Foo.apply)

  // see: http://meta.plasm.us/posts/2013/06/05/applicative-validation-syntax/

  // Applicative validation
  // draw clearer lines between data models and validation logic

  it should "validateFoo" in {
    validateFoo("ab", "cd", "ef") shouldBe
      Failure(NonEmptyList("Not a number!", "Not a lower case letter!", "Wrong size!"))

    validateFoo("42", "cd", "ef") shouldBe
      Failure(NonEmptyList("Not a lower case letter!", "Wrong size!"))

    validateFoo("42", "x", "what") shouldBe
      Success(Foo(42, 'x', "what"))
  }

  it should "transform either using EitherT" in {
    def getInt: Future[String \/ Int] = Future.successful(\/.right(2))
    def getLong(x: Int): Future[String \/ Long] = Future.successful(\/.right(2 * 4L))

    // we should use the EitherT transformer
    val compositionF: Future[String \/ Long] = (for {
      x <- EitherT(getInt)
      y <- EitherT(getLong(x))
    } yield y).run

    val result = compositionF.futureValue
    result shouldBe \/-(8L)
    result.validation shouldBe Success(8)
  }
}
