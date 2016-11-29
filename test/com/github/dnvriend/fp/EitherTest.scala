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
        | x <- success
        |} yield x
      """.stripMargin
    )
  }

  /**
   * Either is cumbersome to use. Whenever you flatMap on an Either you have to decide which of the left and right cases
   * is considered that success case (the so-called left and right projections).
   * This is tedious and, since the right case is always considered the successful case, only serves to introduce bugs.
   * Here’s an example of use, showing the continual need to specify the projection.
   */

  it should "be able to be used in sequence computations" in {
    val result: Either[String, Int] = try {
      Right("foo".toInt)
    } catch {
      case ex: NumberFormatException =>
        Left("Please enter a number")
    }

    val success: Either[String, Int] = Right(1)

    // right biased flatMap, but returns a left
    for (x <- result.right) yield x should matchPattern {
      case Left(_) =>
    }

    // either is also fail fast
    for {
      x <- success.right
      y <- result.right
      z <- success.right
    } yield (x + y + z) should matchPattern {
      case Left(_) =>
    }

    // left biased flatMap
    for (x <- result.left) yield x should matchPattern {
      case x: String =>
    }

    // either is not default biased, we must configure the left or right
    // biased nature of the Either Monad, which is cumbersome
    (for {
      x <- result.right
      y <- result.right
      z <- result.right
    } yield x + y + z) should matchPattern {
      case Left(_) =>
    }
  }
}
