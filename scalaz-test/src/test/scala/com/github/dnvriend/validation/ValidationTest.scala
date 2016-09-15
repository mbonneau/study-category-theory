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

package com.github.dnvriend.validation

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._

object ValidationTest {
  final case class Foo(x: Int, y: Int, z: Int)
}

class ValidationTest extends TestSpec {
  // see: http://eed3si9n.com/learning-scalaz/Validation.html
  // see: http://www.47deg.com/blog/fp-for-the-average-joe-part-1-scalaz-validation
  // see: http://bytes.codes/2015/04/10/a-skeptics-guide-to-scalaz-part-1-disjunctions/
  // see: http://bytes.codes/2015/04/13/a-skeptics-guide-to-scalaz-gateway-drugs-part-2-options-with-disjunction/
  // see: learning scalaz: https://www.youtube.com/watch?v=jyMIvcUxOJ0
  // see: scalaz - the good parts: https://www.youtube.com/watch?v=jPdHQZnF56A
  // see: scalaz - for the rest of us: https://www.youtube.com/watch?v=kcfIH3GYXMI
  // see: scalaz - state monad: https://www.youtube.com/watch?v=Jg3Uv_YWJqI
  // see: A Skeptic's Look at Scalaz Gateway Drugs: https://www.youtube.com/watch?v=lF9OVD0_Boc

  it should 'success in {
    "".successNel shouldBe
      Success("")
  }

  it should 'twoSuccess in {
    ("".successNel[String] *> "".successNel) shouldBe
      Success("")
  }

  it should 'threeSuccess in {
    ("".successNel[String] *> "".successNel *> "".successNel) shouldBe
      Success("")
  }

  it should 'oneFailure in {
    "failure a".failureNel shouldBe
      Failure(NonEmptyList("failure a"))
  }

  it should 'twoFailures in {
    ("failure a".failureNel *> "failure b".failureNel) shouldBe
      Failure(NonEmptyList("failure a", "failure b"))
  }

  it should 'threeFailures in {
    ("failure a".failureNel *> "failure b".failureNel *> "failure c".failureNel) shouldBe
      Failure(NonEmptyList("failure a", "failure b", "failure c"))
  }

  it should 'failSuccessFail1 in {
    ("".successNel[String] *> "failure b".failureNel *> "failure c".failureNel) shouldBe
      Failure(NonEmptyList("failure b", "failure c"))
  }

  it should 'failSuccessFail2 in {
    ("failure a".failureNel *> "".successNel *> "failure c".failureNel) shouldBe
      Failure(NonEmptyList("failure a", "failure c"))
  }

  it should 'failSuccessFail3 in {
    ("failure a".failureNel *> "failure b".failureNel *> "".successNel) shouldBe
      Failure(NonEmptyList("failure a", "failure b"))
  }

  it should 'fromTryCatchNonFatal in {
    (Validation.fromTryCatchNonFatal[Int](1 / 0).leftMap(t ⇒ t.getMessage).toValidationNel[String, Int] *>
      Validation.fromTryCatchNonFatal[Int](1).leftMap(t ⇒ t.getMessage).toValidationNel[String, Int]) shouldBe
      Failure(NonEmptyList("/ by zero"))
  }

  it should "construct a validation from a scala.util.Try success path" in {
    scala.util.Try(1).toValidationNel shouldBe Success(1)
  }

  it should "construct a validation from a scala.util.Try failure path" in {
    val err: Throwable = new RuntimeException("foo")
    scala.util.Try(throw err).toValidationNel *> scala.util.Try(throw err).toValidationNel shouldBe
      Failure(NonEmptyList(err, err))
  }

  it should "validation should accumulate errors on the left side and create a disjunction containing all errors on the left side" in {
    ("err1".failureNel[Unit] *> "err2".failureNel[Unit] *> "err3".failureNel[Unit]).disjunction shouldBe -\/(NonEmptyList("err1", "err2", "err3"))
  }

  it should "validate case class success flow" in {
    case class Foo(x: String)
    Foo("x").successNel[String] *>
      Foo("y").successNel[String] *>
      Foo("z").successNel[String] shouldBe Success(Foo("z"))
  }

  it should "validate case class failure flow" in {
    case class Foo(x: String)
    Foo("x").successNel[String] *>
      "wtf".failureNel *>
      Foo("z").successNel[String] *>
      "rtfm".failureNel shouldBe Failure(NonEmptyList("wtf", "rtfm"))
  }

  it should "fold a list of validation case classes failure flow" in {
    case class Foo(x: String)
    NonEmptyList(
      Foo("x").successNel[String],
      "wtf".failureNel,
      Foo("z").successNel[String],
      "rtfm".failureNel
    ).foldLeft(Foo("x").successNel[String]) {
      case (acc, v) => acc *> v
    } shouldBe Failure(NonEmptyList("wtf", "rtfm"))
  }

  it should "fold a list of validation case classes success flow" in {
    case class Foo(x: String)
    NonEmptyList(
      Foo("x").successNel[String],
      Foo("y").successNel[String],
      Foo("z").successNel[String]
    ).foldLeft(Foo("x").successNel[String]) {
      case (acc, v) => acc *> v
    } shouldBe Success(Foo("z"))
  }

  it should "validate tree of errors" in {
    trait MyFailure
    case class ErrorOne(msg: String) extends MyFailure
    case class ErrorTwo(msg: String) extends MyFailure
    def errorOne(msg: String): MyFailure = ErrorOne(msg)
    def errorTwo(msg: String): MyFailure = ErrorTwo(msg)

    trait MySuccess
    case class SuccessOne(msg: String) extends MySuccess
    case class SuccessTwo(msg: String) extends MySuccess

    Validation.failureNel[MyFailure, MySuccess](ErrorOne("foo")) *> Validation.failureNel(ErrorTwo("bar")) shouldBe Failure(NonEmptyList(ErrorOne("foo"), ErrorTwo("bar")))

    errorOne("foo").failureNel[MySuccess] *> errorTwo("bar").failureNel[MySuccess] shouldBe Failure(NonEmptyList(ErrorOne("foo"), ErrorTwo("bar")))

    SuccessOne("foo").successNel[MyFailure] *> SuccessTwo("bar").successNel[MyFailure] shouldBe Success(SuccessTwo("bar"))
  }

  it should 'FoldingListOfFailures in {
    NonEmptyList(
      "failure a".failureNel[String],
      "failure b".failureNel[String],
      "failure c".failureNel[String]
    ).foldLeft(List.empty[String].successNel[String]) {
      case (acc, v) ⇒ (acc |@| v)(_ :+ _)
    } shouldBe Failure(NonEmptyList("failure a", "failure b", "failure c"))
  }

  it should "accumulate validations using traverseU which is map+sequence" in {
    def validate(msg: String): ValidationNel[String, String] = s"failure $msg".failureNel[String]
    val listToValidate = NonEmptyList("a", "b", "c")
    listToValidate.traverseU(validate) shouldBe
      Failure(NonEmptyList("failure a", "failure b", "failure c"))
  }

  it should "accumulate validations using sequenceU turining a List[ValidationNel[String, A]] into a ValidationNel[String, List[A]] failure case" in {
    List("failure a".failureNel, "failure b".failureNel, "failure c".failureNel).sequenceU shouldBe
      Failure(NonEmptyList("failure a", "failure b", "failure c"))
  }

  it should "accumulate validations using sequenceU turining a List[ValidationNel[String, A]] into a ValidationNel[String, List[A]] success case" in {
    List("success a".successNel[String], "success b".successNel[String], "success c".successNel[String]).sequenceU shouldBe
      Success(List("success a", "success b", "success c"))
  }

  it should "fromTryCatchNonFatal in for comprehension must be a disjunction as Validation is not a Monad but an Applicative" in {
    def throwException: Int = throw new RuntimeException("test")
    (for {
      a ← Validation.fromTryCatchNonFatal(1).disjunction.leftMap(_.getMessage)
      b ← Validation.fromTryCatchNonFatal[Int](throwException).disjunction.leftMap(_.getMessage)
    } yield a + b).validationNel shouldBe Failure(NonEmptyList("test"))
  }

  it should 'validatePaymentAccount in {
    val paymentAccount = PaymentAccount(0, "")
    (PaymentAccountValidation.validateIban(paymentAccount) *>
      PaymentAccountValidation.validateId(paymentAccount)) shouldBe
      Failure(NonEmptyList("iban is empty", "payment account id is invalid, should be > 0"))
  }

  it should 'validatePaymentAccountToCSV in {
    val paymentAccount = PaymentAccount(0, "")
    (PaymentAccountValidation.validateIban(paymentAccount) *> PaymentAccountValidation.validateId(paymentAccount) match {
      case Failure(xs) ⇒ xs.toList.mkString(",")
      case _           ⇒ ""
    }) shouldBe "iban is empty,payment account id is invalid, should be > 0"
  }

  it should "apply the applicative builder for the happy path" in {
    (1.successNel[String] |@| 2.successNel[String] |@| 3.successNel[String]) { (x: Int, y: Int, z: Int) =>
      println(s"Should be called only once with x=$x and y=$y and z=$z")
    }
  }

  it should "create case classes from the result of the applicative builder success path" in {
    (1.successNel[String] |@| 2.successNel[String] |@| 3.successNel[String])(ValidationTest.Foo.apply) shouldBe Success(ValidationTest.Foo(1, 2, 3))
  }

  it should "create case classes from the result of the applicative builder failure path" in {
    ("foo".failureNel[Int] |@| "bar".failureNel[Int] |@| 3.successNel[String])(ValidationTest.Foo.apply) shouldBe Failure(NonEmptyList("foo", "bar"))
  }

  it should "lifting functions into an applicative function using '|@|'" in {
    // see: http://stackoverflow.com/questions/17711895/scalaz-validation-with-applicative-functor-not-working

    // scalaz's applicative functor symbol '|@|' provides a way to lift a function into an applicative functor

    // suppose we have the following results:
    val xs: ValidationNel[String, List[Int]] = "Error!".failureNel
    val ys: ValidationNel[String, List[Int]] = List(1, 2, 3).successNel
    val zs: ValidationNel[String, List[Int]] = List(4, 5).successNel

    // we can lift the concatenation function (_: List[Int]) ++ (_: List[Int]) into the Validation like this:
    val f: (List[Int], List[Int]) ⇒ List[Int] = (_: List[Int]) ++ (_: List[Int])
    (ys |@| zs)(f) shouldBe
      Success(List(1, 2, 3, 4, 5))

    // because scala can infert the types we can inline the function
    (ys |@| zs)(_ ++ _) shouldBe
      Success(List(1, 2, 3, 4, 5))

    (xs |@| ys)(_ ++ _) shouldBe
      Failure(NonEmptyList("Error!"))

    (xs |@| xs)(_ ++ _) shouldBe
      Failure(NonEmptyList("Error!", "Error!"))
  }

  it should "Appending validations using the '+++' operator" in {
    ("Success 1".successNel +++ "Success 2".successNel) shouldBe
      Success("Success 1Success 2")

    ("Success 1".successNel[String] |+| "Success 2".successNel[String]) shouldBe
      Success("Success 1Success 2")

    ("Success 1".successNel[String] |@| "Success 2".successNel[String])(_ |+| _) shouldBe
      Success("Success 1Success 2")

    ("Success 1".successNel[String] |+| "Success 2".successNel[String]) shouldBe
      ("Success 1".successNel[String] |@| "Success 2".successNel[String])(_ |+| _)
  }

  it should "validate a case class" in {
    object Version {
      def validateMajor(major: Int): Validation[String, Int] =
        (major >= 0) ? major.success[String] | "major must be >= 0".failure

      def validateMinor(minor: Int): Validation[String, Int] =
        (minor >= 0) ? minor.success[String] | "minor must be >= 0".failure

      def validate(major: Int, minor: Int): ValidationNel[String, Version] =
        (validateMajor(major).toValidationNel |@| validateMinor(minor).toValidationNel)(Version(_, _))

      def validate(version: Version): ValidationNel[String, Version] =
        validate(version.major, version.minor)

      def create(major: Int, minor: Int): Disjunction[NonEmptyList[String], Version] =
        validate(major, minor).disjunction
    }
    final case class Version(major: Int, minor: Int)

    // lets create a Version the normal way
    val x = Version(1, 2)
    x shouldBe Version(1, 2)
    Version.validate(x) shouldBe Success(x)

    // lets create an invalid version
    val y = Version(-1, -1)
    y shouldBe Version(-1, -1)
    Version.validate(y) shouldBe Failure(NonEmptyList("major must be >= 0", "minor must be >= 0"))
  }
}

object PaymentAccountValidation {
  def validateIban(paymentAccount: PaymentAccount): ValidationNel[String, PaymentAccount] =
    if (Option(paymentAccount.iban).exists(_.isEmpty)) "iban is empty".failureNel else paymentAccount.successNel[String]

  def validateId(paymentAccount: PaymentAccount): ValidationNel[String, PaymentAccount] =
    if (paymentAccount.id <= 0) "payment account id is invalid, should be > 0".failureNel else paymentAccount.successNel[String]
}

case class PaymentAccount(id: Long, iban: String)
