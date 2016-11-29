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

package com.github.dnvriend.scalaz

import com.github.dnvriend.TestSpec

import scalaz._
import scalaz.std.list._
import scalaz.std.option._ // syntax for the Bind type class (and its parents)

class Quickstart extends TestSpec {
  "Functions and list" should "apply" in {
    Apply[Option].apply2(some(1), some(2))((a, b) => a + b) shouldBe Option(3)
    Traverse[List].traverse(List(1, 2, 3))(i => some(i)) shouldBe Option(List(1, 2, 3))
  }
}
