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

package com.github.dnvriend.disjunction

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._
import scala.concurrent.Future

class DisjunctionTest extends TestSpec {

  it should "Disjunction (a scalaz Either) has methods to create the right/left projection of a disjunction" in {
    "Success!".right shouldBe
      \/-("Success!")

    // .left gives problems with ScalaTest's .left
    Disjunction.left("Failure!") shouldBe
      -\/("Failure!")
  }

  it should "The Disjunction Singletion also has the right and left methods" in {
    \/.right("Success!") shouldBe
      \/-("Success!")

    \/.left("Failure!") shouldBe
      -\/("Failure!")
  }

  it should "Fully Symbolic" in {
    \/-("Success!") shouldBe
      \/-("Success!")

    -\/("Failure!") shouldBe
      -\/("Failure!")
  }

  it should "Converting Option to Disjunction" in {
    None.toRightDisjunction("No object found") shouldBe
      -\/("No object found")

    None \/> "No object found" shouldBe
      -\/("No object found")

    Some("My hovercraft is full of eels") \/> "No object found" shouldBe
      \/-("My hovercraft is full of eels")

    Some("My hovercraft is full of eels").toRightDisjunction("No object found") shouldBe
      \/-("My hovercraft is full of eels")
  }

  it should "Converting Disjunction to Option" in {
    \/-(1).toOption shouldBe
      1.some

    -\/("Book not found").toOption shouldBe
      None
  }

  it should "Disjunctions are monads, they are right associated so they fail with the first left, and return only that error message" in {
    (for {
      numOfBooks ← Option(10) \/> "Book not in inventory"
      prize ← Option(22.00) \/> "Book not in prize definition"
    } yield numOfBooks * prize) shouldBe \/-(220.00)

    (for {
      numOfBooks ← none[Int] \/> "Book not in inventory"
      prize ← Option(22.00) \/> "Book not in prize definition"
    } yield numOfBooks * prize) shouldBe -\/("Book not in inventory")

    (for {
      numOfBooks ← Option(10) \/> "Book not in inventory"
      prize ← none[Double] \/> "Book not in prize definition"
    } yield numOfBooks * prize) shouldBe -\/("Book not in prize definition")
  }

  it should "construct disjunction from a code block that throws nonfatal" in {
    val ex: RuntimeException = new RuntimeException("foo")
    \/.fromTryCatchNonFatal {
      throw ex
    } shouldBe -\/(ex)
  }

  it should "construct disjunction from a code block that throws" in {
    val ex: RuntimeException = new RuntimeException("foo")
    \/.fromTryCatchThrowable[String, RuntimeException] {
      throw ex
    } shouldBe -\/(ex)
  }

  it should "construct a disjunction from a scala.util.Try success case" in {
    scala.util.Try(1).toDisjunction shouldBe \/-(1)
  }

  it should "construct a disjunction from a scala.util.Try failure case" in {
    scala.util.Try(1 / 0).toDisjunction should matchPattern { case -\/(_) => }
  }

  it should "append failure side" in {
    \/.left[String, String]("foo") |+| \/.left[String, String]("bar") shouldBe \/.left("foobar")
  }

  it should "append success side" in {
    \/.right[String, String]("foo") |+| \/.right[String, String]("bar") shouldBe \/.right("foobar")
  }

  it should "sequence a list of disjunctions" in {
    // left gives error with the ScalaTest types
    List(Disjunction.left("foo"), Disjunction.left("bar"), "baz".right[String]).sequenceU shouldBe -\/("foo")
  }

  it should "sequence a list of ValidationNel result in a single (failed) Validation accumulating all errors" in {
    List("foo".failureNel[String], "bar".failureNel[String], "baz".successNel[String]).sequenceU shouldBe Failure(NonEmptyList("foo", "bar"))
  }

  it should "sequence a list of ValidationNel result in a single (success) Validation accumulating all successes" in {
    List("foo".successNel[String], "bar".successNel[String], "baz".successNel[String]).sequenceU shouldBe Success(List("foo", "bar", "baz"))
  }
  it should "sequence a list of ValidationNel result in a single (failed) Validation accumulating all errors converting to a left-disjunction" in {
    List("foo".failureNel[String], "bar".failureNel[String], "baz".successNel[String]).sequenceU.disjunction shouldBe -\/(NonEmptyList("foo", "bar"))
  }

  it should "sequence a list of ValidationNel result in a single (success) Validation accumulating all errors converting to a right-disjunction" in {
    List("foo".successNel[String], "bar".successNel[String], "baz".successNel[String]).sequenceU.disjunction shouldBe \/-(List("foo", "bar", "baz"))
  }

  it should "Converting Disjunction to Validation" in {
    \/-("Success!").validationNel[String] shouldBe
      Success("Success!")

    -\/("Failure!").validationNel[String] shouldBe
      Failure(NonEmptyList("Failure!"))
  }

  it should "Converted Validations can be folded failure case" in {
    NonEmptyList(
      \/-("Success 1").validationNel[String],
      \/-("Success 2").validationNel[String],
      -\/("Failure 1").validationNel[String],
      -\/("Failure 2").validationNel[String],
      \/-("Success 3").validationNel[String],
      \/-("Success 4").validationNel[String]
    ).foldLeft(List.empty[String].successNel[String]) {
      case (acc, v) ⇒ (acc |@| v)(_ :+ _)
    } shouldBe Failure(NonEmptyList("Failure 1", "Failure 2"))
  }

  it should "Converted Validations can be folded success case" in {
    NonEmptyList(
      \/-("Success 1").validationNel[String],
      \/-("Success 2").validationNel[String],
      \/-("Success 3").validationNel[String],
      \/-("Success 4").validationNel[String]
    ).foldLeft(List.empty[String].successNel[String]) {
      case (acc, v) ⇒ (acc |@| v)(_ :+ _)
    } shouldBe Success(List("Success 1", "Success 2", "Success 3", "Success 4"))
  }

  it should "map a left side of the disjunction to a type" in {
    val x: Future[String \/ Int] = Future.successful(1.right[String])
    x.flatMap {
      case \/-(right) => Future.successful(right)
      case -\/(left)  => Future.failed(new RuntimeException(left))
    }.futureValue shouldBe 1

    // .left gives problems with ScalaTest's .left
    Future.successful(Disjunction.left("foo")).flatMap {
      case \/-(right) => Future.successful(right)
      case -\/(left)  => Future.failed(new RuntimeException(left))
    }.toTry should be a 'failure
  }

  "Disjunctions without symbols" should "be created for the left case" in {
    Disjunction.left[NonEmptyList[String], String](NonEmptyList("foo")) shouldBe NonEmptyList("foo").left
  }

  it should "be created for the right case" in {
    Disjunction.right[NonEmptyList[String], String]("foo") shouldBe "foo".right
  }

  "DisjunctionNel" should "be created" in {
    // note: DisjunctionNel is *NOT* part of Scalaz, strange as a Disjunction[NonEmptyList[String], A] is a very
    // common pattern when working with validation ie. "".failureNel.disjunction would create one..
    // The TestSpec.scala contains a type alias called DisjunctionNel

    // Also, TestSpec.scala contains two implicit conversions to create .leftNel and .rightNel DisjunctionNel types
    // just like the .left/.right methods

    "foo".leftNel[String] shouldBe a[DisjunctionNel[String, String]]
    "foo".leftNel[String] shouldBe NonEmptyList("foo").left
    "foo".rightNel shouldBe "foo".right
  }

  it should "be used on methods" in {
    // note: .toNel on String is *NOT* part of Scalaz, it is provided by an extension method
    // in TestSpec.scala. Sometimes it is convenient to convert a String to NonEmptyList by means
    // of an extension method to make it compatible with the DisjunctionNel[String, A] pattern.
    def strToInt(number: String): DisjunctionNel[String, Int] =
      Disjunction.fromTryCatchNonFatal(number.toInt)
        .leftMap(cause => s"Error while converting '$number' to Int".toNel)

    // lets convert some 'numbers', or are they?
    List("aa", "bb")
      .map(strToInt) // get the disjunction
      .traverseU(_.validation) // convert to a List[ValidationNel[String, Int]] with map/sequence combo called 'traverseU'
      .disjunction shouldBe // convert to disjunction
      NonEmptyList("Error while converting 'aa' to Int", "Error while converting 'bb' to Int").left
  }

  // of course using Validation as return type for a validation is better

  "Validation to validate" should "be used on methods" in {
    def strToLong(number: String): ValidationNel[String, Long] =
      Disjunction.fromTryCatchNonFatal(number.toLong)
        .leftMap(cause => s"Error while converting '$number' to Long".toNel)
        .validation

    // lets convert some 'numbers', or are they?
    List("aa", "bb")
      .traverseU(strToLong) // convert to a List[ValidationNel[String, Long]] with map/sequence combo called 'traverseU'
      .disjunction shouldBe // convert to disjunction
      NonEmptyList("Error while converting 'aa' to Long", "Error while converting 'bb' to Long").left
  }
}
