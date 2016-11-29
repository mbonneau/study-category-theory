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

import scalaz.syntax.std.option._

class OptionTest extends TestSpec {

  "composing options" should "be much simpler" in {
    (for {
      x <- 1.some ? 2.some | 3.some
    } yield x).value shouldBe 2
  }

  it should "be complex without scalaz" in {
    (for {
      x <- Option(1).flatMap(_ => Option(2)).orElse(Option(3))
    } yield x).value shouldBe 2
  }

  "options" should "not be matched on" in {
    val someOption: Option[Int] = 1.some
    def foo(a: Int): Unit = ()

    someOption match {
      case Some(a) => foo(a)
      case None    => ()
    }
  }

  /**
   * this code works, but it has some problems:
   * - too literal, unsophisticated,
   * - not high enough level of abstraction,
   * - not declarative enough, procedural
   * it feels like:
   *
   *   // if (Some(a)) foo(a) else if(None) ()
   *
   * higher order functions available on the Option are the best things we have
   * so lets use them!
   *
   * Pattern matching on Option is almost never necessary.
   *
   * Usually there are cleaner and more expressive ways to attain the same results
   *
   * The most idiomatic way to use an scala.Option is to treat it as a collection monad and use
   * map, flatMap, filter and foreach.
   *
   * A less idiomatic way to use scala.Option values is via pattern matching.
   *
   * Using HOF is not a matter of coding style
   *
   */

  "idiomatic Option" should "not pattern matched" in {
    val someOption: Option[Int] = 1.some
    def foo(a: Int): Int = a * 2

    someOption match {
      case Some(a) => foo(a)
      case None    => 1
    }

    // should be
    someOption map foo getOrElse 1

    // or
    someOption.fold(1)(foo)
    // the fold option is more type strict than the other two
  }

  it should "use isDefined" in {
    1.some.fold(false)(_ => true) shouldBe true

    // can become
    1.some.isDefined shouldBe true
  }

  it should "use map" in {
    def f(a: Int): Int = a * 2
    1.some match {
      case Some(a) => Some(f(a))
      case None    => None
    }
    // this is equivalent to
    1.some map f
  }

  it should "use getOrElse" in {
    1.some match {
      case Some(a) => a
      case None    => 2
    }

    // this is equivalent to
    1.some getOrElse 2
  }

  it should "use fold" in {
    def f(a: Int): Int = a * 2
    1.some match {
      case Some(a) => f(a)
      case None    => 2
    }

    // this is equivalent to
    1.some.fold(2)(f)
  }

  it should "use isEmpty" in {
    1.some match {
      case Some(a) => false
      case _       => true
    }

    // use
    1.some.isEmpty
  }

  it should "use nonEmpty or isDefined" in {
    1.some match {
      case Some(a) => true
      case _       => false
    }

    // use
    1.some.isDefined
    // or
    1.some.nonEmpty
  }

  it should "use size" in {
    1.some match {
      case Some(a) => 1
      case None    => 0
    }

    // use
    1.some.size
  }

  it should "use orNull" in {
    1.some match {
      case Some(a) => a
      case _       => null
    }

    // err, well, don't use null but if you have to use
    1.some.orNull
  }

  it should "use filter" in {
    def p(a: Int): Boolean = false
    1.some match {
      case Some(a) if p(a) => Some(a)
      case _               => None
    }

    // use
    1.some filter p
    //or
    1.some find p
  }

  it should "use filterNot" in {
    def p(a: Int): Boolean = false
    1.some match {
      case Some(a) if !p(a) => Some(a)
      case _                => None
    }

    // use
    1.some filterNot p
  }

  it should "use contains" in {
    1.some match {
      case Some(a) => a == 1
      case _       => false
    }

    // use
    1.some contains 1
  }

  it should "use exists" in {
    def p(a: Int): Boolean = false
    1.some match {
      case Some(a) => p(a)
      case None    => false
    }

    // use
    1.some exists p

    // eg to test whether the string "banana" contains an "a"
    "banana".some exists (_ contains "a")
  }

  it should "use forall" in {
    def p(a: Int): Boolean = false
    1.some match {
      case Some(a) => p(a)
      case None    => true
    }

    // use
    1.some forall p
  }

  it should "use count" in {
    def p(a: Int): Boolean = false
    1.some match {
      case Some(a) if p(a) => 1
      case _               => 0
    }

    // use count
    //
  }

  it should "use foreach" in {
    1.some match {
      case Some(a) => ()
      case _       => ()
    }

    // use foreach
    1.some foreach (_ => ())
  }
}
