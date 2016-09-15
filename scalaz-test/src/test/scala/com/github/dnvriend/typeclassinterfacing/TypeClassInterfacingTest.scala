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

package com.github.dnvriend.typeclassinterfacing

import com.github.dnvriend.TestSpec

class TypeClassInterfacingTest extends TestSpec {

  // There are three (3) ways of interfacing Typeclass instances to clients:
  // 1. Interface objects,
  // 2. Extension methods,
  // 3. Operation classes

  // All three methods use the trick:
  // 1. use a method
  // 2. parameterize the method, eg. with 'A'
  // 3. Call the method with a value, which will lock the 'A' to the type of the caller
  // 4. Use an implicit typeclass that will be parameterized with 'A'
  // 5. The compiler will look for a typeclass to be injected and must be in implicit scope that
  //    has the same type as the locked 'A' type, eg. if A has been locked to the type Int, then
  //    the compiler will look for a typeclass INSTANCE that has been parameterized with Int.

  //
  // Interfacing typeclass instances is defined as:
  // "Generic methods that accept instances of the type class as an implicit parameter"
  //

  // 1. Interface objects: a.k.a. "All methods in a singleton"

  trait AddOne[A] { def addOne(a: A): A }
  object DefaultAddOnes {
    implicit val intAddOne = new AddOne[Int] { override def addOne(a: Int): Int = a + 1 }
    implicit val longAddOne = new AddOne[Long] { override def addOne(a: Long): Long = a + 1 }
    implicit val stringAddOne = new AddOne[String] { override def addOne(a: String): String = a + "one" }
  }

  object AddOne {
    // the implicit parameter AddOne will look for the typeclass of type A because the addOne method
    // has been parameterized with A, and because the value is of type A, the looked for AddOne typeclass
    // will be 'injected' if in implicit scope, cool!!
    def addOne[A](value: A)(implicit adder: AddOne[A]): A = adder.addOne(value)
  }

  "Strategy 1 - interface object" should "add one" in {
    import DefaultAddOnes._
    AddOne.addOne(1) shouldBe 2
    AddOne.addOne(1L) shouldBe 2L
    AddOne.addOne("foo") shouldBe "fooone"
  }

  // 2. Interface syntax: a.k.a. "pimp existing types with interface methods" a.k.a. "extension methods" (SIP-13)
  // see: http://docs.scala-lang.org/sips/completed/implicit-classes.html

  object AddOneSyntax {
    implicit class AddOneExtension[A](value: A) {
      def addOne(implicit adder: AddOne[A]): A = adder.addOne(value)
    }
  }

  "Strategy 2 - (SIP-13) extension methods a.k.a. pimp-my-type" should "add one" in {
    import DefaultAddOnes._
    import AddOneSyntax._
    1.addOne shouldBe 2
    1L.addOne shouldBe 2L
    "foo".addOne shouldBe "fooone"
  }

  // 3. The Scalaz way a.k.a. Using the type class in an xxxOps class. The xxxOps class use the type class (that will be
  //    passed in implicitly and provide more methods

  sealed trait ToAddOneOps {
    implicit def toAddOneOps[A](v: A)(implicit F: AddOne[A]): AddOneOps[A] =
      new AddOneOps(v)
  }

  class AddOneOps[A](v: A)(implicit adder: AddOne[A]) {
    def addOne: A = adder.addOne(v)
  }

  object addones extends ToAddOneOps

  "Strategy 3 - Ops classes a.k.a 'ops-class-use-typeclass-and-providing-more-methods'" should "add one" in {
    import DefaultAddOnes._
    import addones._
    1.addOne shouldBe 2
    1L.addOne shouldBe 2L
    "foo".addOne shouldBe "fooone"
  }
}
