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

import cats.Traverse
import cats.instances.all._
import cats.syntax.traverse._

class TraverseTest extends TestSpec {
  it should "sequence a list" in {
    Traverse[List].sequence(List(Option(1), Option(2))) shouldBe Some(List(1, 2))
    List(Option(1), Option(2)).sequenceU shouldBe Some(List(1, 2))
  }

  it should "traverse a list" in {
    def toOption[A](value: A): Option[A] = Option(value)
    List(1, 2).traverseU(toOption) shouldBe Some(List(1, 2))
  }
}
