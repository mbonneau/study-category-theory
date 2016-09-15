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
import com.github.dnvriend.typeclass.EqualTest._

import scalaz.Equal
import scalaz.std.AllInstances._

object EqualTest {
  final case class Foo(a: Int, b: Double, c: Boolean)
  implicit val fooEqual = new Equal[Foo] {
    override def equal(a1: Foo, a2: Foo): Boolean =
      a1.equals(a2)
  }
}

class EqualTest extends TestSpec {
  def withEqual[A: Equal](f: Equal[A] => Unit): Unit =
    f(implicitly[Equal[A]])

  it should "equal Int (more verbose)" in {
    implicitly[Equal[Int]].equal(1, 1) shouldBe true
  }

  it should "equal Int (less verbose by means of context bound)" in withEqual[Int] { tc =>
    tc.equal(1, 1) shouldBe true
  }

  it should "equal Int (symbolic)" in {
    1 === 1 shouldBe true
  }

  it should "equal string (all-in-one)" in withEqual[String] { tc =>
    implicitly[Equal[String]].equal("a", "b") shouldBe false
    tc.equal("a", "b") shouldBe false
    tc.equal("a", "a") shouldBe true
    "a" === "a" shouldBe true
  }

  it should "use a custom FooEqual typeclass to check Foo for equality" in withEqual[Foo] { tc =>
    tc.equals(Foo(1, 2, false), Foo(1, 2, false))
    Foo(1, 2, false) === Foo(1, 2, false)
  }
}
