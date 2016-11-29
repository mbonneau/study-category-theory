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

import com.github.dnvriend.TestSpec

import io.github.hamsters.Validation
import io.github.hamsters.Validation._

class ValidationTest extends TestSpec {

  "Validation" should "give failures and successes" in {
    val e1 = OK(1)
    val e2 = KO("nan")
    val e3 = KO("nan2")

    val validation = Validation(e1, e2, e3)
    validation.failures should be(List("nan", "nan2"))
  }

  "Either" should "compose using flatMap and map" in {

    val e1 = OK(1)
    val e2 = OK(2)
    val e3 = OK(3)

    val combine = for {
      v1 <- e1
      v2 <- e2
      v3 <- e3
    } yield s"$v1-$v2-$v3"

    combine should be(OK("1-2-3"))

  }

  "Either" should "stop at first error" in {

    val e1: Either[String, Int] = OK(1)
    val e2: Either[String, Int] = KO("nan")
    val e3: Either[String, Int] = KO("nan2")

    val combine = for {
      v1 <- e1
      v2 <- e2
      v3 <- e3
    } yield s"$v1-$v2-$v3"

    combine should be(KO("nan"))

  }
}
