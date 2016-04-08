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

import scalaz._
import Scalaz._
import scala.util.Try

class ValidationTest extends TestSpec with DisjunctionMatchers {

  /**
   * Take a look at: https://www.parleys.com/tutorial/a-skeptics-look-scalaz-gateway-drugs
   */

  // a quick domain (don't you just love Scala?)
  case class Address(city: String)
  case class User(first: String, last: String, address: Option[Address])
  case class DBObject(id: Long, user: Option[User])

  /**
   * Validation:
   * - it looks similar to Disjunction \/ and you can convert between them
   * - it's subtypes are scalaz.Success and scalaz.Failure
   * - it's *not* a Monad!!
   * - it's an Applicative Functor (it adds stuff together, it chains operations)
   * - If any failure in the chain, failure wins: All errors get mashed together
   *
   * Validation will be used in a chain of Validations, if any failure happens, the failure will win,
   * all the "errors will be mashed together".
   */

  val brendan = DBObject(1, Option(User("Brendan", "McAdams", None)))
  val someOtherGuy = DBObject(2, None)
  val brendanCA = DBObject(4, Option(User("Brendan", "McAdams", Option(Address("Sunnyvale")))))
  val cthulu = DBObject(5, Option(User("Cthulu", "Old One", Option(Address("R'lyeh")))))
  val noSuchPerson = DBObject(6, None)
  val jonPretty = DBObject(7, Option(User("Jon", "Pretty", None)))

  /**
   * We can now validate these users:
   *  - Is there a user object?
   *  - Is there an address object?
   */

  def validDBUser(dbObj: DBObject): Validation[String, User] = dbObj.user match {
    case Some(user) ⇒ Success(user)
    case _          ⇒ Failure(s"DBObject $dbObj does not contain a user object")
  }

  "Validate DB Object" should "be able to validate on user" in {
    validDBUser(brendanCA) should be a 'success
    validDBUser(cthulu) should be a 'success
    validDBUser(noSuchPerson) should be a 'failure
    validDBUser(jonPretty) should be a 'success
  }

  def postOfficeValid(address: Address): Boolean = address match {
    case Address(city) if city == "R'lyeh" ⇒ false
    case _                                 ⇒ true
  }

  def validAddress(user: Option[User]): Validation[String, Address] = user match {
    case Some(User(_, _, Some(address))) if postOfficeValid(address) ⇒ address.success
    case Some(User(_, _, Some(address)))                             ⇒ s"Invalid address: address: ${address.city} not recognized by postal service".failure
    case Some(user @ User(_, _, None))                               ⇒ s"User: ${user.first} ${user.last} has no defined address".failure
    case None                                                        ⇒ "No such user".failure
  }

  "Validate User" should "be able to validate on address" in {
    validAddress(brendanCA.user) should be a 'success
    validAddress(cthulu.user) should be a 'failure
    validAddress(noSuchPerson.user) should be a 'failure
    validAddress(jonPretty.user) should be a 'failure
  }

  /**
   * Applicative operators: add stuff together
   *
   * Scalaz has a number of applicative operators that combine results:
   *
   * "*>" and "<*":
   * - "*>" takes the right hand value and discards the left,
   * - "<*" takes the left hand value and discards the right.
   * - Errors always win
   */

  "applicative *>" should "combine left + right but discard left" in {
    1.some *> 2.some shouldBe Some(2)
    None *> 2.some should not be 'defined
  }

  "applicative <*" should "combine left + right but discard right" in {
    1.some <* 2.some shouldBe Some(1)
    1.some <* None should not be 'defined
  }

  def validateUser(dbuser: DBObject): Validation[String, Address] =
    validDBUser(dbuser) *> validAddress(dbuser.user)

  "validating DB User" should "brendanCA to success" in {
    validateUser(brendanCA) should be a 'success
  }

  it should "cthulu to failure" in {
    validateUser(cthulu) should be a 'failure
    validateUser(cthulu) should matchPattern {
      case Failure("Invalid address: address: R'lyeh not recognized by postal service") ⇒
    }
  }

  it should "Jon Pretty to failure" in {
    validateUser(jonPretty) should be a 'failure
    validateUser(jonPretty) should matchPattern {
      case Failure("User: Jon Pretty has no defined address") ⇒
    }
  }

  it should "no such person to failure" in {
    validateUser(noSuchPerson) should be a 'failure
    validateUser(noSuchPerson) should matchPattern {
      case Failure("DBObject DBObject(6,None) does not contain a user objectNo such user") ⇒
    }

    // !! Note, the "*>" on Validation appends all the errors together
  }

  /**
   * Enter NonEmptyList (NEL)
   *
   * - NonEmptyList (NEL) is just a List which is guaranteed to have at least one element.
   * - You cannot instantiate a NEL without an element!
   * - It is commonly used with Validation to allow accrual of multiple error messages
   * - It is so commonly used in fact, that there exists a simple type alias for a Validation that contains a NonEmptyList
   *   - Validation[NonEmptyList[L], R] => ValidationNEL[L, R] which is nice :)
   * - Append on an NEL will add each element separately
   */

  /**
   * We can be explicit and construct a NonEmptyList, and also declare the types explicitly
   */
  def validDBUserNELExplicit(dbObj: DBObject): Validation[NonEmptyList[String], User] = dbObj.user match {
    case Some(user) ⇒ Success(user)
    case _          ⇒ Failure(NonEmptyList(s"DBObject $dbObj does not contain a user object"))
  }

  /**
   * Or we can use some helpful shortcuts, call .failureNel and declare the ValidationNel type
   */
  def validDBUserNel(dbObj: DBObject): ValidationNel[String, User] = dbObj.user match {
    case Some(user) ⇒ Success(user)
    case _          ⇒ s"DBObject $dbObj does not contain a user object".failureNel
  }

  def validAddressNel(user: Option[User]): ValidationNel[String, Address] = user match {
    case Some(User(_, _, Some(address))) if postOfficeValid(address) ⇒ address.success
    case Some(User(_, _, Some(address)))                             ⇒ s"Invalid address: address: ${address.city} not recognized by postal service".failureNel
    case Some(user @ User(_, _, None))                               ⇒ s"User: ${user.first} ${user.last} has no defined address".failureNel
    case None                                                        ⇒ "No such user".failureNel
  }

  def validateUserNel(dbObj: DBObject): ValidationNel[String, Address] =
    validDBUserNel(dbObj) *> validAddressNel(dbObj.user)

  "validating using NEL" should "no such person to failure" in {
    validateUserNel(noSuchPerson) should matchPattern {
      case Failure(NonEmptyList(_, _)) ⇒
    }
  }

  /**
   * A new applicative operator, the |@| or 'Admiral Ackbar'
   *
   * 'Admiral Ackbar' combines all of the success and all of the failure conditions,
   * but we must provide a PartialFunction to combine them
   */

  "combining two validations" should "produce a new output" in {
    val result: ValidationNel[String, String] = (validDBUserNel(brendanCA) |@| validAddressNel(brendanCA.user)) {
      case (user, address) ⇒
        s"User ${user.first} ${user.last} lives in ${address.city}"
    }
    result should be a 'success
    result should matchPattern {
      case Success("User Brendan McAdams lives in Sunnyvale") ⇒
    }
  }

  it should "combine three validations" in {
    val result: ValidationNel[String, String] = (validDBUserNel(brendanCA) |@| validDBUserNel(brendanCA) |@| validDBUserNel(brendanCA)) {
      case (user1, user2, user3) ⇒ s"${user1.first},${user2.first},${user3.first}"
    }
    result should be a 'success
    result should matchPattern {
      case Success("Brendan,Brendan,Brendan") ⇒
    }
  }

  /**
   * Dealing with Errors is always a challenge, but there are a few ways in Scala:
   *  - we can use the try/catch, but let's don't use that,
   *  - we can use scala.util.Try that materialized to a Success or a Failure,
   *  - but there is also a .fromTryCatchTrowable higher order function on Disjunction \/,
   *    - this will catch any exception you specify,
   *    - and return a disjunction
   *    - you must specify a return type
   *    - and you must specify the types of exceptions to catch,
   *    - and then a function body; something to call
   */

  "dealing with errors" should "not deal with NumberFormatException when nothing is done about it" in {
    intercept[NumberFormatException] {
      "foo".toInt
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
  }
}
