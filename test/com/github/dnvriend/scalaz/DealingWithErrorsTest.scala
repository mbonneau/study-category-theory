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
import org.typelevel.scalatest.DisjunctionMatchers

import scala.util.Try
import scalaz.Scalaz._
import scalaz._

class DealingWithErrorsTest extends TestSpec with DisjunctionMatchers {

  /**
   * Dealing with Errors is always a challenge, but there are a few ways in Scala:
   * - we can use the try/catch, but let's don't use that,
   * - we can use scala.util.Try that materialized to a Success or a Failure,
   * - but there is also a .fromTryCatchTrowable higher order function on Disjunction \/,
   * - this will catch any exception you specify,
   * - and return a disjunction
   * - you must specify a return type
   * - and you must specify the types of exceptions to catch,
   * - and then a function body; something to call
   */

  "dealing with errors" should "not deal with NumberFormatException when nothing is done about it" in {
    intercept[NumberFormatException] {
      "foo".toInt
    }
  }

  it should "deal with errors using a Try" in {
    Try("foo".toInt) should be a 'failure
  }

  it should "deal with errors using a Disjunction" in {
    val result: NumberFormatException \/ Int = \/.fromTryCatchThrowable[Int, NumberFormatException] {
      "foo".toInt
    }
    result should be(left)
  }

  it should "deal with errors when non fatal else they will be thrown" in {
    \/.fromTryCatchNonFatal[Int] {
      "foo".toInt
    } should be(left)
  }

  /**
   * Resources: http://underscore.io/blog/posts/2015/02/23/designing-fail-fast-error-handling.html
   *
   * Exploring error handling techniques in Scala. Like in any design some goals must be formulated:
   *
   * 1. Stop as soon as we encounter an error also known as "Fail Fast". We don't want to accumulate all the errors,
   * which is a validation problem, and leads to a different solution. So fail fast makes it *impossible* to do any
   * error accumulation.
   * 2. Guarantee to handle every error we intend to handle; using the Scala type system to guarantee that
   * code that does not implement error handling will not compile.
   *
   * There are consequences:
   * 1. if there are errors we don’t care to handle, perhaps because they are so unlikely, or
   * we cannot take any action other than crashing, don’t model them; and
   * 2. if we add or remove an error type that we do want to handle, the compiler must force us to update the code.
   *
   * There are two elements to our design:
   *
   * 1. how we represent the act of encountering an error (to give us fail-fast behaviour); and
   * 2. how we represent the information we store about an error.
   *
   * Failing fast:
   * Our two tools for fail-fast behaviour are "throwing exceptions" and "sequencing computations using monads".
   *
   * We can immediately discard using exceptions. Exceptions are unchecked in Scala, meaning the compiler will not
   * force us to handle them. Hence they won’t meet our second goal.
   *
   * This leaves us with Monads. The term may not be familiar to all Scala programmers, but most will be familiar
   * with Option and "flatMap". This is essentially the behaviour we are looking for. Option gives us fail-fast
   * behaviour when we use flatMap to sequence computations.
   */

  "Option" should "give fail fast semantics using flatMap to sequence computations" in {
    (for {
      x <- 1.some
      y <- none[Int]
      z <- 3.some
    } yield x + y + z) should not be 'defined
  }

  /**
   * There are a lot of data structures that implement variations of the fail-fast idea. We might also use Either or Try
   * from the standard library, or Scalaz’s disjuction, written \/.
   *
   * We want some information on errors for debugging. This means we can immediately drop Option from consideration,
   * as when we encounter an error the result is simply None. We know that an error has happened, but we don’t know
   * what error it is.
   *
   * We can also drop Try from consideration. Try always stores a Throwable to represent errors. What is a Throwable?
   * It can be just about anything. In particular, it’s not a sealed trait so the compiler can’t help us to ensure we
   * handle all the cases we intend to handle. Therefore we can’t meet goal two if we use Try.
   */

  /**
   *  The preferred choice is Scalaz’s \/ (Disjunction) type, which is right-biased. This means it always considers
   *  the right hand to be the successful case for flatMap and map. It’s much more convenient to use than Either and
   *  can be used as a mostly drop-in replacement for Either. Here’s an example of use.
   */

  "Scalaz disjunction at work" should "handle an error" in {
    val result: String \/ Int = try {
      \/.right("foo".toInt)
    } catch {
      case ex: NumberFormatException =>
        \/.left("Please enter a number")
    }

    (for {
      x <- result
      y <- result
      z <- result
    } yield x + y + z) should be(left)

    // or shorter
    val result2: Throwable \/ Int = \/.fromTryCatchNonFatal[Int] { "foo".toInt }
    (for (x <- result2) yield x) should be(left)
  }

  /**
   * Representing errors:
   * Having decided to use the disjunction Monad for fail-fast error handling, let’s turn to how we represent errors.
   *
   * Errors form a logical disjunction. For example, database access could fail because the record is not found or no
   * connection could be made or we are not authenticated, and so on. As soon as we see this structure we should turn to
   * an algebraic data type (a sum type in particular), which we implement in Scala with code like:
   *
   * sealed trait DatabaseError
   * final case class NotFound(...) extends DatabaseError
   * final case class CouldNotConnect(...) extends DatabaseError
   * final case class CouldNotAuthenticate(...) extends DatabaseError
   *
   * When we process a DatabaseError we will typically use a match expression, and because we have used a sealed trait
   * the compiler will tell us if we have forgotten a case. This meets our second goal, of handling every error we intend to handle.
   *
   * It is strongly recommended defining a separate error type for each logical subsystem. Defining a system wide error hierarchy
   * quickly becomes unwieldy, and you often want to expose different information at different layers of the system. For example,
   * it is useful to include authentication information if a login fails but making this information available in our HTTP service
   * could lead to leaking confidential information if we make a programming error.
   */

}
