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

import cats.Semigroup
import cats.instances.all._
import cats.syntax.semigroup._

class SemigroupTest extends TestSpec {
  it should "combine two ints" in {
    val intSemi = Semigroup[Int]
    intSemi.combine(1, intSemi.combine(2, 3)) shouldBe intSemi.combine(intSemi.combine(1, 2), 3)
  }

  it should "append two values" in {
    (1 |+| 2) |+| 3 shouldBe 1 |+| (2 |+| 3)
  }

  it should "merge maps" in {
    val map1 = Map("hello" -> List(0), "world" -> List(1))
    val map2 = Map("hello" -> List(2), "cats" -> List(3))

    map1 |+| map2 shouldBe Map("hello" -> List(0, 2), "world" -> List(1), "cats" -> List(3))
  }
}
