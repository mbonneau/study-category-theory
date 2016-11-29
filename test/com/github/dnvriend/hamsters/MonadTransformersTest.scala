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

package com.github.dnvriend.hamsters

import com.github.dnvriend.TestSpec
import io.github.hamsters.Validation._
import io.github.hamsters.{ FutureEither, FutureOption }
import io.github.hamsters.MonadTransformers._

import scala.concurrent.Future

class MonadTransformersTest extends TestSpec {
  "FutureOption" should "handle Future[Option[_]] type" in {
    def foa: Future[Option[String]] = Future(Some("a"))
    def fob(a: String): Future[Option[String]] = Future(Some(a + "b"))

    val composedAB: Future[Option[String]] = (for {
      a <- FutureOption(foa)
      ab <- FutureOption(fob(a))
    } yield ab).future

    composedAB.futureValue shouldBe Some("ab")

    val composedABWithNone: Future[Option[String]] = (for {
      a <- FutureOption(Future.successful(None))
      ab <- FutureOption(fob(a))
    } yield ab).future

    composedABWithNone.futureValue shouldBe None

    val composedABWithFailure: Future[Option[String]] = (for {
      a <- FutureOption(Future.failed(new Exception("d'oh!")))
      ab <- FutureOption(fob(a))
    } yield ab).future

    an[Exception] should be thrownBy composedABWithFailure.futureValue
  }

  "FutureOption" should "be filtered with pattern matching in for comprehension" in {
    def fo: Future[Option[(String, Int)]] = Future(Some(("a", 42)))

    val filtered = (for {
      (a, i) <- FutureOption(fo) if i > 5
    } yield a).future

    filtered.futureValue shouldBe Some("a")

    val filtered2 = (for {
      (a, i) <- FutureOption(fo) if i > 50
    } yield a).future

    filtered2.futureValue should not be defined
  }

  "FutureEither" should "handle Future[Either[_,_]] type" in {
    def fea: Future[Either[String, Int]] = Future(OK(1))
    def feb(a: Int): Future[Either[String, Int]] = Future(OK(a + 2))

    val composedAB: Future[Either[String, Int]] = (for {
      a <- FutureEither(fea)
      ab <- FutureEither(feb(a))
    } yield ab).future

    composedAB.futureValue shouldBe OK(3)

    val composedABWithNone: Future[Either[String, Int]] = (for {
      a <- FutureEither(Future.successful(KO("d'oh!")))
      ab <- FutureEither(feb(a))
    } yield ab).future

    composedABWithNone.futureValue shouldBe KO("d'oh!")

    val composedABWithFailure: Future[Either[String, Int]] = (for {
      a <- FutureEither(Future.failed(new Exception("d'oh!")))
      ab <- FutureEither(feb(a))
    } yield ab).future

    an[Exception] should be thrownBy composedABWithFailure.futureValue
  }

  "FutureEither" should "be filtered with pattern matching in for comprehension" in {
    def fe: Future[Either[String, (String, Int)]] = Future(OK(("a", 42)))

    val filtered = (for {
      (a, i) <- FutureEither(fe) if i > 5
    } yield a).future

    filtered.futureValue shouldBe OK("a")

    val filtered2 = (for {
      (a, i) <- FutureEither(fe) if i > 50
    } yield a).future

    filtered2.futureValue shouldBe KO("No value matching predicate")
  }
}
