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

package com.github.dnvriend.list

import com.github.dnvriend.TestSpec

import scalaz.Scalaz._
import scalaz._

class ListTest extends TestSpec {

  // legend
  // The symbol '@' is an ApplicativeBuilder
  // (f1 |@| f2 |@| ... |@| fn)((v1, v2, ... vn) => ...)
  // (f1 |@| f2 |@| ... |@| fn).tupled`
  // The ApplicativeBuilder has the '.tupled' method
  // that returns the result as a tuple, of course, in the
  // given Higher Kinded Type (HKT) like eg. a List

  it should 'IndexedList in {
    List(3, 4, 5).indexed shouldBe
      List((0, 3), (1, 4), (2, 5))
  }

  it should 'allPairs in {
    List(1, 2, 3).allPairs shouldBe
      List((1, 2), (2, 3), (1, 3))
    List(1, 2, 3, 4).allPairs shouldBe
      List((1, 2), (2, 3), (3, 4), (1, 3), (2, 4), (1, 4))
  }

  it should 'adjacentPairs in {
    List(1, 2, 3).adjacentPairs shouldBe
      List((1, 2), (2, 3))
    List(1, 2, 3, 4).adjacentPairs shouldBe
      List((1, 2), (2, 3), (3, 4))
  }

  it should 'intersperse in {
    List(1, 2, 3).intersperse(0) shouldBe
      List(1, 0, 2, 0, 3)
  }

  it should 'powerset in {
    List(1, 2).powerset shouldBe
      List(List(1, 2), List(1), List(2), List())
    List(1, 2, 3).powerset shouldBe
      List(List(1, 2, 3), List(1, 2), List(1, 3), List(1), List(2, 3), List(2), List(3), List())
  }

  it should 'initz in {
    List(1, 2).initz shouldBe
      List(List(), List(1), List(1, 2))
    List(1, 2, 3).tailz shouldBe
      List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  it should 'tailz in {
    List(1, 2).tailz shouldBe
      List(List(1, 2), List(2), List())
    List(1, 2, 3).tailz shouldBe
      List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  it should 'lookup in {
    // returns the first tuple's value that matches the key '1'
    List((1, 2), (2, 3), (1, 3)).lookup[Int, Int](1) shouldBe
      Some(2)
    // returns the first tuple's value that matches the key '2'
    List((1, 2), (2, 3), (1, 3)).lookup[Int, Int](2) shouldBe
      Some(3)
    // returns the first tuple's value that matches the key '3'
    List((1, 2), (2, 3), (1, 3)).lookup[Int, Int](3) shouldBe
      None
  }

  it should 'toNel in {
    List(1, 2, 3).toNel shouldBe
      NonEmptyList(1, 2, 3).some
  }

  it should 'tailOption in {
    List(1, 2, 3).tailOption shouldBe
      Some(List(2, 3))
  }

  it should 'crossProductTupledApplicative in {
    // The result of |@| is an
    (List(1, 2) |@| List(3, 4)).tupled shouldBe
      List((1, 3), (1, 4), (2, 3), (2, 4))
    (List(1, 2) |@| List(3, 4)).tupled.map { case (l, r) ⇒ l.toString |+| r.toString } shouldBe
      List("13", "14", "23", "24")
  }

  it should 'crossProductWithInts in {
    (List(1, 2) |@| List(3, 4))((v1, v2) ⇒ v1.toString |+| v2.toString) shouldBe
      List("13", "14", "23", "24")
  }

  it should 'crossProductWithChars in {
    val charGen = ('A' to 'B').toList
    (charGen |@| charGen)((l, r) ⇒ l.toString |+| r.toString) shouldBe
      List("AA", "AB", "BA", "BB")
    (charGen |@| charGen)(_.toString |+| _.toString).mkString(",") shouldBe
      "AA,AB,BA,BB"
  }

  it should 'crossProductOption in {
    (1.some |@| 2.some).tupled shouldBe
      (1, 2).some
  }

  it should 'ThreeListsApplicative in {
    (List(1, 2) |@| List(3, 4) |@| List(5, 6)).tupled shouldBe
      List((1, 3, 5), (1, 3, 6), (1, 4, 5), (1, 4, 6), (2, 3, 5), (2, 3, 6), (2, 4, 5), (2, 4, 6))
    (List(1, 2) |@| List(3, 4) |@| List(5, 6))((v1, v2, v3) ⇒ v1 + v2 + v3).sum shouldBe
      84
    (List(1, 2) |@| List(3, 4) |@| List(5, 6))(_ |+| _ |+| _).sum shouldBe
      84
  }
}
