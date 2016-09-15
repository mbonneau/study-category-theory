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

package com.github.dnvriend.typeclass

import com.github.dnvriend.TestSpec
import com.github.dnvriend.typeclass.ShowTest._

import scalaz._
import Scalaz._

object ShowTest {
  final case class Foo(a: Int, b: Double, c: Boolean)
  implicit val fooShow = new Show[Foo] {
    override def shows(f: Foo): String = {
      import f._
      s"""
         |===============
         |case class Foo:
         |===============
         |a=$a,
         |b=$b,
         |c=$c
       """.stripMargin
    }
  }
}

class ShowTest extends TestSpec {
  def withShow[A: Show](f: Show[A] => Unit): Unit =
    f(implicitly[Show[A]])

  it should "show int" in {
    implicitly[Show[Int]].shows(10) shouldBe "10"
    10.shows shouldBe "10"
  }

  it should "use a Show typeclass for Foo" in withShow[Foo] { tc =>
    tc.shows(Foo(1, 2, c = false)) should not be 'empty
  }
}
