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

package com.github.dnvriend.hamsters

import org.scalatest.{ FlatSpec, Matchers }

import io.github.hamsters.Validation
import Validation._

class OkKoTest extends FlatSpec with Matchers {
  info(
    """
      | ############################################
      | # Validation and monadic OK/KO
      | ############################################
      | #
      | Statements can be OK or KO. Then you can get all successes and failures.
      |
    """.stripMargin
  )

  "OK/KO instances" should "be monadic" in {
    val e1: Either[String, Int] = OK(1)
    val e2: Either[String, Int] = KO("nan")
    val e3: Either[String, Int] = KO("nan2")

    // Stop at first error
    val result = for {
      v1 <- e1
      v2 <- e2
      v3 <- e3
    } yield s"$v1-$v2-$v3"

    result shouldBe KO("nan")
  }

  it should "throw when the .get method is used" in {
    val e1: Either[String, Int] = KO("nan")
    intercept[java.util.NoSuchElementException] {
      e1.get
    }
  }

  it should "catch exceptions in a KO object" in {
    def compute(x: Int): Int = 2 / x

    Validation.fromCatchable(compute(1)) shouldBe OK(2)
    Validation.fromCatchable(compute(0)) shouldBe KO("/ by zero")
    Validation.fromCatchable(compute(0), (t: Throwable) => t.getClass.getSimpleName) shouldBe KO("ArithmeticException")
  }

  "Validation" should "accrue / aggregate / concat / the OK/KO instances" in {
    val e1: Either[String, Int] = OK(1)
    val e2: Either[String, Int] = KO("error 1")
    val e3: Either[String, Int] = KO("error 2")

    // Validation only has a hasFailures and
    val validation: Validation[String] = Validation(e1, e2, e3)
    validation.hasFailures shouldBe true
    validation.failures shouldBe List("error 1", "error 2")

    val successes: Validation[String] = Validation(e1)
    successes.hasFailures shouldBe false
    successes.failures shouldBe 'empty
  }
}
