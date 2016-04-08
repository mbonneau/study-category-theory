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

package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class EitherTest extends TestSpec {
  /**
   * Scala's builtin `Either` is a commonly used tool, allowing `Left` and 'Right` projections,
   *
   * By convention, `Left` indicates an error while `Right` indicates a success,
   */

  "Either" should "be a success" in {
    val success = Right("Success!")
    success shouldBe 'isRight
    success.isRight shouldBe a[java.lang.Boolean]
    success should not be 'isLeft
    success.isLeft shouldBe a[java.lang.Boolean]

    // because Right is not a Monad (no .flatMap) it won't compile!
    // this is an epic fail because we love for comprehensions (and monads)
    //
    // it is not easy to extract (or pass which way you look at it)
    // from/to the modules (Monads)
    //
    // lets try scalaz.Disjunction! (so go to the DisjunctionTest)
    assertDoesNotCompile(
      """
        |val success = Right("Success!")
        |for {
        | x ← success
        |} yield x
      """.stripMargin)
  }
}
