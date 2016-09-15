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

package com.github.dnvriend.nel

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._

class NonEmptyListTest extends TestSpec {

  it should "create a NonEmptyList" in {
    NonEmptyList(1) shouldBe NonEmptyList(1)
    1.wrapNel shouldBe NonEmptyList(1) // from NelOps
    1.point[NonEmptyList] shouldBe NonEmptyList(1)
  }
}
