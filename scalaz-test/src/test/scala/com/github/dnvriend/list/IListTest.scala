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

class IListTest extends TestSpec {
  // legend:
  // '==>' comes from uTest and is an 'Arrow Assert'.
  // Arrow assert is a nice syntax for asserting things are equal.
  // 'a ==> b' is a shorthand for assert(a == b)

  //  it should "IList is invariant, the following should not compile" in {
  //       IList is invariant; these won't compile
  //      compileError(""""abc" :: IList(1, 2, 3)""")
  //      compileError("""IList.empty[String] ++ IList(1, 2, 3)""")
  //    }

  it should "Construct using elements" in {
    IList(1, 2, 3) shouldBe
      IList(1, 2, 3)
  }

  it should "Construct using cons" in {
    1 :: 2 :: 3 :: INil() shouldBe
      IList(1, 2, 3)
  }

  it should "Construct from list" in {
    IList.fromList(List(1, 2, 3)) shouldBe
      IList(1, 2, 3)
  }

  it should "Construct from Option" in {
    IList.fromOption(Some(2)) shouldBe
      IList(2)
  }

  it should "Construct from fill" in {
    IList.fill(5)(1) shouldBe
      IList(1, 1, 1, 1, 1)
  }

  it should "Empty list" in {
    // Empty IList
    INil[String]() shouldBe
      IList()
    IList.empty[String] shouldBe
      IList()
  }

  it should "Widen the list type to eg. 'Any'" in {
    "abc" :: IList(1, 2, 3).widen[Any] shouldBe
      IList("abc", 1, 2, 3)
  }

  it should "List operations" in {
    IList(1, 2).reverse shouldBe
      IList(2, 1)
    IList(1, 2).foldLeft(0)(_ + _) shouldBe
      3
  }

  it should "IList are total" in {
    IList(1, 2).tailOption shouldBe
      IList(2).some
    IList.empty[Int].tailOption shouldBe
      None
    IList(1, 2).reduceLeftOption(_ + _) shouldBe
      3.some
  }

  it should 'Deconstructing in {
    IList(1, 2).uncons("empty", (h, t) ⇒ "head is %s and tail is %s".format(h, t)) shouldBe
      "head is 1 and tail is [2]"
  }

  it should "Deconstructing with match" in {
    (IList(1, 2) match {
      case INil()      ⇒ "empty"
      case ICons(h, t) ⇒ "head is %s and tail is %s".format(h, t)
    }) shouldBe
      "head is 1 and tail is [2]"
  }

  it should 'Products in {
    (IList(1, 2) |@| IList(true, false)).tupled shouldBe
      IList((1, true), (1, false), (2, true), (2, false))
  }

  it should 'unit in {
    33.point[IList] shouldBe
      IList(33)
  }

  it should 'less in {
    (IList(1, 2, 3) < IList(1, 2, 4)) shouldBe
      true
  }

  it should 'traverse in {
    IList(1, 2, 3).traverse(_.some) shouldBe
      IList(1, 2, 3).some
  }

  it should 'conversion in {
    IList(1, 2).toList shouldBe
      List(1, 2)
    IList(1, 2).toVector shouldBe
      Vector(1, 2)
    IList(1, 2).toStream shouldBe
      Stream(1, 2)
  }

  it should 'ephemeralStream in {
    val estr = IList(1, 2).toEphemeralStream
  }

  it should 'toMap in {
    // 'shouldBe>' is an immutable map of key/value pairs implemented as a balanced binary tree
    val zmap = IList(1, 2).map(n ⇒ (n, "x" * n)).toMap
    zmap.lookup(1) shouldBe
      "x".some
    zmap.lookup(2) shouldBe
      "xx".some
  }
}
