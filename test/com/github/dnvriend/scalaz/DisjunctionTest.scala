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
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException
import org.typelevel.scalatest.{ DisjunctionMatchers, DisjunctionValues }

import scalaz._
import Scalaz._
import scalaz.syntax.either._

class DisjunctionTest extends TestSpec with Matchers with DisjunctionValues with DisjunctionMatchers {

  /**
   * Yes, Validations are the gateway drug: https://www.parleys.com/tutorial/a-skeptics-look-scalaz-gateway-drugs
   * Scalaz Disjunction is so cool it has its own operator! "\/"
   *
   * "\/" assumes we mostly want the right 'success' value, so its 'right' biased,
   *
   * It unpacks in for comprehensions / map / flatMap where the 'positive' "\/-" value
   * 'continues', and 'negative' "-\/" aborts.
   */

  /**
   * Best practice: when declaring types, prefer infix notation eg:
   */

  def queryInfixNotation[A](arg: String): Failure[String] \/ Success[A] = ???

  /**
   * vs the standard notation
   */

  def queryStandardNotation[A](arg: String): \/[Failure[String], Success[A]] = ???

  /**
   * Once the whole Scalaz universe has been imported :) we can invoke .right and .left on any value
   * to get a Disjunction
   */

  "Disjunction" should "be created and tested" in {
    // see: https://github.com/typelevel/scalaz-scalatest
    // DisjunctionValues assumes the right-bias of \/ in your test. If it isn't right, then the
    // test will fail with a [TestFailedException]
    val success: Nothing \/ String = \/.right("Success!")
    success.value shouldBe "Success!"
    success should be(right)
    success should be(\/-)
    success should not be left

    val failure: String \/ Nothing = \/.left("Failure!")
    intercept[TestFailedException] {
      failure.value shouldBe "Failure!"
    }
    failure should be(left)
    failure should be(-\/)
    failure should not be right
  }

  /**
   * There are also postfix .left and .right that allow us to convert an existing value to a disjunction,
   * but does not work in a ScalaTest though, so we are using the "\/.right" and "\/.left" operators,
   * which makes it (in my opinion) a little bit clearer that the type will be a disjunction, which is just
   * calling the constructor on the disjunction object "\/".
   */

  // a quick domain (don't you just love Scala?)
  case class Address(city: String)
  case class User(first: String, last: String, address: Option[Address])
  case class DBObject(id: Long, user: Option[User])

  "comprehend over options" should "find a user, because dao returned a user" in {
    val brendan = Option(DBObject(1, Option(User("Brendan", "McAdams", None))))
    val result: Option[User] = for {
      dao <- brendan
      user <- dao.user
    } yield user

    result shouldBe 'defined
  }

  it should "not find a user, because the dao did not return a user" in {
    val someOtherGuy = Option(DBObject(2, None))
    val result: Option[User] = for {
      dao <- someOtherGuy
      user <- dao.user
    } yield user

    result should not be 'defined // but what went wrong??
  }

  /**
   * Downside of Option:
   *  - comprehending over groups of options lead to "silent failure",
   *  - we do not want a silent failure, we want to know what went wrong!
   *
   * Solution:
   *  - Scalaz includes implicits to help convert an Option to a Disjunction
   *  - \/ is right-biased makes it easy to comprehend over
   *  - On a left we'll get potentially useful information instead of None, which is silent (by design).
   *  - The left side can have a value, which can be a message, an Exception or something else that can tell us what went wrong.
   */

  /**
   * Scalaz includes implicits to help convert an Option to a Disjunction, and because its Scalaz, you can
   * use symbols or methods, its up to you to decide the level of abstraction.
   */

  /**
   * Scalaz can try to convert an Option instance, which can be a None or a Some, to a "right-disjunction".
   * The result will be a right-disjunction when the Option is an instance of Some. If the Option is an instance of
   * None (which is silent), you must give the message, Exception, or some other value that will be the left-disjunction.
   *
   * Let's see it in action
   */

  "scalaz can convert Option types to Disjunction" should "convert to right disjunction" in {
    //
    // The gist is: convert the Some to a Right, but if you cannot, use this message for the Left
    //
    val eelDisjunction: String \/ String = Some("My hovercraft is full of Eels!") toRightDisjunction "No object found"
    eelDisjunction should be(right)

    val scratchedDisjunction: String \/ String = Some("I will not buy this record it is scratched") \/> "No object found"
    scratchedDisjunction should be(right)
  }

  it should "convert to left disjunction" in {
    //
    // The gist is: convert the None to a Right, but if you cannot, use this message for the Left
    //
    val resultFromNone1: String \/ Nothing = None \/> "No object found"
    resultFromNone1 should be(left)

    val resultFromNone2: String \/ Nothing = None toRightDisjunction "No object found"
    resultFromNone2 should be(left)
  }

  /**
   * When looking at the dao example, we suddenly have more useful information
   */

  "option to disjunction in dao" should "find a user, because dao returned a user" in {
    val brendan = Option(DBObject(1, Option(User("Brendan", "McAdams", None))))
    val result: String \/ User = for {
      dao <- brendan \/> "No user by that ID"
      user <- dao.user \/> "Join failed: no user object"
    } yield user

    result should be(right)
  }

  it should "not find a user, because the dao did not return a user" in {
    val someOtherGuy = Option(DBObject(2, None))
    val result: String \/ User = for {
      dao <- someOtherGuy \/> "No user by that ID"
      user <- dao.user \/> "Join failed: no user object"
    } yield user

    result should be(left)
  }

  /**
   * Because Scalaz can convert the silent Option to a Disjunction with a message when its None, we can
   * capture the context, and produce a *valuable* error message, we can use in a lot more *powerful*
   * ways!
   *
   * Skip to Validation
   */

}
