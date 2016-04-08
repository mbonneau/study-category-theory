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
import Scalaz._

class OptionTest extends TestSpec {

  "composing options" should "be much simpler" in {
    (for {
      x ← 1.some ? 2.some | 3.some
    } yield x).value shouldBe 2
  }

  it should "be complex without scalaz" in {
    (for {
      x ← Option(1).flatMap(_ ⇒ Option(2)).orElse(Option(3))
    } yield x).value shouldBe 2
  }
}
