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

package com.github.dnvriend.implicits

import org.scalatest.{ FlatSpec, Matchers }

object ImplicitsTest {
  implicit val z = "-bar"
}

object Foo {
  implicit def toFoo(that: Baz): Foo = Foo(that.x)
}
case class Foo(x: String) {
  def ++(that: Foo) = Foo(this.x + that.x)
}

object Bar {
  implicit def toFoo(that: Bar): Foo = Foo(that.x)
}
case class Bar(x: String)

object Baz {
  implicit val addable = new Add[Baz] {
    override def add(f1: Baz, f2: Baz): Baz = Baz(f1.x + f2.x)
  }

  implicit val double = new Double[Baz] {
    override def double(f1: Baz, f2: Baz): Baz = Baz(f1.x + f1.x + f2.x + f2.x)
  }
}
case class Baz(x: String)

// encapsulates knowledge (the proof if you will)
// that we can add two things that are of the same type
trait Add[A] {
  def add(f1: A, f2: A): A
}

// encapsulate knowledge how we double things of the same type
trait Double[A] {
  def double(f1: A, f2: A): A
}

class ImplicitsTest extends FlatSpec with Matchers {

  it should "compute using implicit parameter injection using local scope" in {
    implicit val y = "-foo"
    def doSomething(x: Int)(implicit y: String): String = x + y
    doSomething(1) shouldBe "1-foo"
  }

  it should "compute using implicit parameter injection using explicit import" in {
    import ImplicitsTest.z
    def doSomething(x: Int)(implicit y: String): String = x + y
    doSomething(1) shouldBe "1-bar"
  }

  it should "compute using implicit parameter injection using wildcard import" in {
    import ImplicitsTest._
    def doSomething(x: Int)(implicit y: String): String = x + y
    doSomething(1) shouldBe "1-bar"
  }

  it should "(1) look for implicits in the (companion object o/t) source type which is Bar here" in {
    // this method has a higher priority than the one below
    //
    // why is the source type Bar? In this computation, we start with Foo that has a method called 'def ++(that: Foo),
    // which means Foo expects that it can be added to another Foo.
    // So the computation is Foo ++ Foo
    // But we are doing a Foo ++ Bar type yes? This does not compute.. at first
    // the compiler will look for options to *make* this compute
    //
    // how can we make this compute? Well by converting the Bar to a Foo..
    // It will first do this by looking to Bar (the part of the computation that is is 'wrong')
    // which is called the 'source'. It will look into the companion object for this type is there is
    // an implicit conversion available to use to convert the Bar to a Foo, which there is..
    // it will apply the conversion and evaluate the expression
    Foo("x") ++ Bar("y") shouldBe Foo("xy")
  }

  it should "(2) look for implicits in the (companion object o/t) expected type which is Baz here" in {
    // this method has a lower priority than the one above
    //
    // same story here, but Baz does not have a conversion from Baz => Foo, so what to do.
    // the compiler will now look for an implicit conversion method in the expected type, which is Foo
    Foo("x") ++ Baz("z") shouldBe Foo("xz")
  }

  it should "Look for implicits in companion objects of type argument" in {
    // what does this even mean? We need this for the type class pattern.
    // I have borrowed this sentence from the fantastic book 'shapeless-guide'
    // https://github.com/underscoreio/shapeless-guide and is a very accessible book in my opinion
    // and I will admit, its a book that depending on your knowledge level must be read multiple times
    // to fully ingest all the information it contains, but its a well worth read and a good exercise for the brain :)

    // Type classes are a programming pa ern borrowed from Haskell (the word “class” has **nothing** (absolutely nothing) to do with classes
    // in object oriented programming). We encode them in Scala using traits and implicits.
    // A type class is a parametrized trait representing some sort of general functionality that we would like to
    //apply to a wide range of types.

    // lets define a generic method
    def add[A](x: A, y: A)(implicit ev: Add[A]): A = ev.add(x, y)

    // When called with a type, the generic method will look for an implicit Addable-of-that-type.
    // it will use the resolution strategy like 'local scope', explicit imports, wildcard imports,
    // and because none is found, will look in the companion object of the reified type argument, and
    // when type A is eg. in this case a Baz, it will look in the companion object of Baz for an Addable
    // to add two Baz types..

    add(Baz("Hello "), Baz("World!")) shouldBe Baz("Hello World!")

    // when using a method, we can use type classes to add properties to the computation
    // when looking at it this way, type classes get a whole new meaning
    // lets add the 'features', Adding and Doubling types to the following method

    def crazy[A](x: A, y: A)(implicit adder: Add[A], doubler: Double[A]): A =
      adder.add(doubler.double(x, y), doubler.double(x, y))

    crazy(Baz("x"), Baz("y")) shouldBe Baz("xxyyxxyy")
  }
}
