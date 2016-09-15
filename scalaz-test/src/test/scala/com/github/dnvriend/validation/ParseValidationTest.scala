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

import scalaz._
import Scalaz._

class ParseValidationTest extends TestSpec {
  it should "parse values and return validations which is part of 'scalaz.syntax.std.StringOps'" in {
    "true".parseBoolean shouldBe Success(true)
    "false".parseBoolean shouldBe Success(false)
    "TRUE".parseBoolean shouldBe Success(true)
    "FALSE".parseBoolean shouldBe Success(false)
    "FOO".parseBoolean should matchPattern { case Failure(t: IllegalArgumentException) => }
    "6".parseInt shouldBe 6.success
    "6".parseLong shouldBe 6L.success
    "6".parseDouble shouldBe 6.0.success
    "6".parseBigInt shouldBe BigInt(6).success
    "6".parseBigDecimal shouldBe BigDecimal(6.0).success

    "foo".parseInt should matchPattern { case Failure(t: NumberFormatException) => }
    "foo".parseLong should matchPattern { case Failure(t: NumberFormatException) => }
  }
}
